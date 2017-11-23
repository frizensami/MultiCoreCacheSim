module MESIProtocol (MESIState (..), load, store) where

import Bus (CacheBus, BusTr (..))
import qualified Bus
import Cache (Cache)
import qualified Cache
import CacheBlock (BlockState (..))
import Definitions
import Memory (Memory)
import qualified Memory
import Protocol

data MESIState = MESIWaitCacheRead | MESIWaitCacheWrite
    | MESIWaitBusTr
    | MESIWaitMemoryRead | MESIWaitMemoryWrite
    | MESIWaitCacheRewrite | MESIDone deriving (Show)

instance ProtocolState MESIState where
    isDone MESIDone = True
    isDone _        = False

-- |Loads a memory address to the processor while adhering to the MESI coherence protocol.
--  Returns the renewed MESIState, cache, memory, and cache bus.
load :: Maybe MESIState -> MemoryAddress -> Cache -> Memory -> CacheBus -> Int -> (MESIState, Cache, Memory, CacheBus)
load Nothing memoryAddress cache memory cacheBus pid = (newMESIState, newCache, memory, cacheBus) where
    newMESIState = MESIWaitCacheRead
    newCache = Cache.issueRead memoryAddress cache

-- Load on WaitCacheRead.
-- Could proceed to IssueBusTr/WaitBusTr/WaitMemoryRead/Done/WaitCacheRead depending on the cache/bus states.
load (Just MESIWaitCacheRead) memoryAddress cache memory cacheBus pid = (newMESIState, newCache, newMemory, newCacheBus) where
    cacheHit = Cache.isCacheHit cache

    (isBusAcquired, cacheBusAfterAcquire) = case cacheHit of
        Just True   -> (False, cacheBus) -- Cache hit, no need to issue bus transaction
        Just False  -> Bus.acquire (MESIBusRd memoryAddress) cacheBus pid -- Cache miss, try to issue a bus transaction through acquire
        Nothing     -> (False, cacheBus) -- Cache read not finished yet, no need to issue bus transaction on this cycle

    newMESIState = case cacheHit of
        Just True   -> MESIDone -- Cache hit, load operation is done
        Just False  -> case isBusAcquired of -- Cache miss, next state depends on the bus state after acquire attempt
            False   -> MESIWaitCacheRead -- Bus can't be acquired, keep trying to acquire on WaitCacheRead state
            True    -> case Bus.isBusy cacheBusAfterAcquire of
                True    -> MESIWaitBusTr -- Bus is busy, wait for bus transaction to finish
                False   -> MESIWaitMemoryRead -- Bus transaction is instant, do a memory read and go to memory read state
        Nothing     -> MESIWaitCacheRead -- Cache read not finished yet, wait until it is finished

    newCache = case cacheHit of
        Just True   -> case blockState of
            Just _  -> Cache.commitRead memoryAddress cache -- Cache hit, do a commit read
            Nothing -> cache
            where
                blockState = Cache.busGetBlockState memoryAddress cache
        Just False  -> cache -- Cache miss, no changes to cache
        Nothing     -> cache -- Cache read not finished yet, no changes to cache

    newMemory = case cacheHit of
        Just True   -> memory -- Cache hit, no memory operation needed
        Just False  -> case isBusAcquired of -- Cache miss, memory might be modified depending on the bus state after acquire attempt
            False   -> memory -- Bus can't be acquired, no memory modification while trying to acquire bus
            True    -> case Bus.isBusy cacheBusAfterAcquire of -- Bus acquired, memory might be modified depending on whether bus transaction is instant
                True    -> memory -- Bus is busy, no memory modification until bus transaction is finished
                False   -> Memory.issueRead memory -- Bus transaction is instant, do a memory read
        Nothing     -> memory -- Cache read not finished yet, no memory operation needed

    newCacheBus = cacheBusAfterAcquire -- Returns the acquired cache bus if the bus is acquired

-- Load on WaitBusTr, this state could only be reached if the bus transaction issued was not instant (a cache has the address cached on M state, for example).
-- Could proceed to WaitBusTr/WaitMemoryRead depending on the bus state.
load (Just MESIWaitBusTr) memoryAddress cache memory cacheBus pid = (newMESIState, cache, newMemory, cacheBus) where
    isBusBusy = Bus.isBusy cacheBus

    newMESIState = case isBusBusy of
        True    -> MESIWaitBusTr -- Bus is still busy, wait for it in WaitBusTr state.
        False   -> MESIWaitMemoryRead -- Bus is no longer busy, do a memory read and go to memory read state

    newMemory = case isBusBusy of
        True    -> memory -- Bus is still busy, no changes to memory while waiting for bus.
        False   -> Memory.issueRead memory -- Bus is no longer busy, do a memory read.

-- Load on WaitMemoryRead.
-- Could proceed to WaitMemoryRead/WaitMemoryWrite/Done depending on the memory state and cache allocation result.
load (Just MESIWaitMemoryRead) memoryAddress cache memory cacheBus pid = (newMESIState, newCache, newMemory, newCacheBus) where
    isMemoryBusy = Memory.isBusy memory

    (maybeEvictedBlockState, newCache) = case isMemoryBusy of
        True    -> (Nothing, cache) -- Memory is still busy, no allocation done on this cycle
        False   -> case Bus.hasSharedCopies memoryAddress cacheBus of -- Memory is no longer busy, do allocation on local cache
            True    -> Cache.busAllocate S memoryAddress cache -- Cache block is shared, allocate in S state
            False   -> Cache.busAllocate E memoryAddress cache -- Cache block is not shared, allocate in E state

    newMESIState = case isMemoryBusy of
        True    -> MESIWaitMemoryRead -- Memory is still busy, wait for it to complete
        False   -> case maybeEvictedBlockState of -- Memory is no longer busy, next state depends on whether allocation results in eviction of M cache block
            Just M      -> MESIWaitMemoryWrite -- An M cache block was evicted, needs to be written back to memory, wait for this memory write
            _           -> MESIDone -- No M cache block was evicted, go to done state

    newMemory = case isMemoryBusy of
        True    -> memory -- Memory is still busy, do not issue any memory operation
        False   -> case maybeEvictedBlockState of -- Memory is no longer busy, a memory write might need to be done due to M block eviction
            Just M      -> Memory.issueWrite memory -- An M cache block was evicted, needs to be written back to memory
            _           -> memory -- No M cache block was evicted, no more memory operations issued

    newCacheBus = case isMemoryBusy of
        True    -> cacheBus -- Memory is still busy, no changes to cache bus
        False   -> case maybeEvictedBlockState of -- Memory is no longer busy, cache bus might need to be released depending on whether Done state is reached.
            Just M      -> cacheBus -- An M cache block was evicted, Done state not yet reached, don't release the cache bus
            _           -> Bus.release cacheBus -- No M cache block was evicted, Done state reached, release the cache bus

-- Load on WaitMemoryWrite, this state could only be reached if an M state cache block was evicted during cache allocation.
-- Could proceed to WaitMemoryWrite/Done state depending on the memory state.
load (Just MESIWaitMemoryWrite) memoryAddress cache memory cacheBus pid = (newMESIState, cache, memory, newCacheBus) where
    isMemoryBusy = Memory.isBusy memory

    newMESIState = case isMemoryBusy of
        True    -> MESIWaitMemoryWrite -- Memory write of evicted block is still running, wait until memory write is complete
        False   -> MESIDone -- Memory write of evicted block is complete, go to done state

    newCacheBus = case isMemoryBusy of
        True    -> cacheBus -- Memory write of evicted block is still running, no changes to cache bus on this cycle
        False   -> Bus.release cacheBus -- Memory write of evicted block is complete, Done state reached, release the cache bus

load (Just MESIWaitCacheWrite) _ _ _ _ _ = error "WaitCacheWrite state on MESIProtocol.load"
load (Just MESIWaitCacheRewrite) _ _ _ _ _ = error "WaitCacheRewrite state on MESIProtocol.load"
load (Just MESIDone) _ _ _ _ _ = error "Done state on MESIProtocol.load"

-- |Stores a memory address from the processor while adhering to the MESI coherence protocol.
--  Returns the renewed MESIState, cache, memory, and cache bus.
store :: Maybe MESIState -> MemoryAddress -> Cache -> Memory -> CacheBus -> Int -> (MESIState, Cache, Memory, CacheBus)
store Nothing memoryAddress cache memory cacheBus pid = (newMESIState, newCache, memory, cacheBus) where
    newMESIState = MESIWaitCacheWrite
    newCache = Cache.issueWrite memoryAddress cache

-- Store on WaitCacheWrite.
-- Could proceed to Done/IssueBusTr/WaitBusTr/WaitMemoryRead/WaitCacheWrite depending on the bus states and whether it is a cache hit.
store (Just MESIWaitCacheWrite) memoryAddress cache memory cacheBus pid = (newMESIState, newCache, newMemory, newCacheBus) where
    cacheHit = Cache.isCacheHit cache
    blockState = Cache.busGetBlockState memoryAddress cache

    (isBusAcquired, cacheBusAfterAcquire) = case cacheHit of
        Just True   -> case blockState of -- Cache hit, bus transaction might be issued depending on the block state
            Just M      -> (False, cacheBus) -- Cache hit on M state, no need to acquire the bus
            Just E      -> (False, cacheBus) -- Cache hit on E state, no need to acquire the bus
            Just S      -> Bus.acquire (MESIBusUpg memoryAddress) cacheBus pid -- Cache hit on S state, issue a MESIBusUpg transaction to invalidate other copies
            _           -> Bus.acquire (MESIBusRdX memoryAddress) cacheBus pid -- Cache hit on I state, block changed through bus by other protocol -> MESIBusRdX
        Just False  -> Bus.acquire (MESIBusRdX memoryAddress) cacheBus pid -- Cache miss, issue a MESIBusRdX transaction to invalidate other copies and read from memory
        Nothing     -> (False, cacheBus) -- Cache write has not completed, no need to acquire the bus on this cycle

    newMESIState = case cacheHit of
        Just True   -> case blockState of -- Cache hit, next state depends on the block state
            Just M      -> MESIDone -- Cache hit on M state, immediately go to Done state
            Just E      -> MESIDone -- Cache hit on E state, immediately go to Done state
            Just S      -> case isBusAcquired of -- Cache hit on S state, next state depends on whether bus is acquired
                False   -> MESIWaitCacheWrite -- Bus not acquired, keep trying to acquire on WaitCacheWrite state
                True    -> MESIDone -- Bus acquired, invalidation is instant, go to Done state
            _           -> case isBusAcquired of -- Cache hit on I state, block changed through bus, next state depends on whether the bus is acquired
                False   -> MESIWaitCacheWrite -- Bus not acquired, keep trying to acquire on WaitCacheWrite state
                True    -> case Bus.isBusy cacheBusAfterAcquire of -- Bus acquired, next state depends on whether bus transaction is instant
                    True    -> MESIWaitBusTr -- Bus is busy (transaction not instant), go to WaitBusTr state 
                    False   -> MESIWaitMemoryRead -- Bus transaction is instant, issue memory read and go to WaitMemoryRead state
        Just False  -> case isBusAcquired of -- Cache miss, next state depends on whether bus is acquired
            False   -> MESIWaitCacheWrite -- Bus not acquired, keep trying to acquire on IssueBusTr state
            True    -> case Bus.isBusy cacheBusAfterAcquire of -- Bus acquired, next state depends on whether bus transaction is instant
                True    -> MESIWaitBusTr -- Bus is busy (transaction not instant), go to WaitBusTr state
                False   -> MESIWaitMemoryRead -- Bus transaction is instant, issue memory read and go to WaitMemoryRead state
        Nothing     -> MESIWaitCacheWrite

    newCache = case cacheHit of
        Just True   -> case blockState of
            Just M      -> Cache.commitWrite memoryAddress cache -- Cache hit on M block, commit write
            Just E      -> Cache.commitWrite memoryAddress cache -- Cache hit on E block, commit write
            Just S      -> case isBusAcquired of
                False   -> cache -- Bus can't be acquired, do not commit write on this cycle to maintain original cache state
                True    -> Cache.commitWrite memoryAddress cache -- Bus acquired, commit write is now allowed
            _           -> cache -- Cache hit on I state, block changed through bus, no changes to cache
        Just False  -> cache -- Cache miss, no changes to cache
        Nothing     -> cache -- Cache is still busy, no changes to cache

    newMemory = case cacheHit of
        Just True   -> memory -- Cache hit, no memory operation needed
        Just False  -> case isBusAcquired of -- Cache miss, might issue a memory read depending on the bus state after acquire attempt
            False   -> memory -- Bus can't be acquired, no memory operation issued while trying to acquire bus
            True    -> case Bus.isBusy cacheBusAfterAcquire of -- Bus acquired, memory might be modified depending on whether bus transaction is instant
                True    -> memory -- Bus is busy, no memory operation issued until bus transaction is finished
                False   -> Memory.issueRead memory -- Bus transaction is instant, issue a memory read
        Nothing     -> memory -- Cache read not finished yet, no memory operation needed

    newCacheBus = case cacheHit of
        Just True   -> case blockState of
            Just M      -> cacheBus -- Cache hit on M state, no bus transaction generated, return the same bus
            Just E      -> cacheBus -- Cache hit on E state, no bus transaction generated, return the same bus
            Just S      -> case isBusAcquired of -- Cache hit on S state, new bus depends on whether the bus is acquired successfully
                False   -> cacheBus -- Bus has not been acquired, return the same bus
                True    -> Bus.release cacheBusAfterAcquire -- Bus has been acquired, will not be busy (only invalidate), release acquired bus
            _           -> cacheBusAfterAcquire -- Cache hit on I state, block changed through bus, returns the acquired cache bus if any
        Just False  -> cacheBusAfterAcquire -- Cache miss, returns the acquired cache bus if the bus is acquired
        Nothing     -> cacheBus -- Cache read not finished yet, no change to cache bus

-- Store on WaitBusTr, this state could only be reached if the issued bus transaction was not instant.
-- Could proceed to WaitBusTr/WaitMemoryRead depending on whether the bus is still busy.
store (Just MESIWaitBusTr) memoryAddress cache memory cacheBus pid = (newMESIState, cache, newMemory, cacheBus) where
    isBusBusy = Bus.isBusy cacheBus

    newMESIState = case isBusBusy of
        True    -> MESIWaitBusTr -- Bus is busy, wait for the bus completion in WaitBusTr state
        False   -> MESIWaitMemoryRead -- Bus is no longer busy, issue memory read and go to WaitMemoryRead state

    newMemory = case isBusBusy of
        True    -> memory -- Bus is busy, no memory read issued this cycle
        False   -> Memory.issueRead memory -- Bus is no longer busy, issue memory read

-- Store on WaitMemoryRead.
-- Could proceed to WaitMemoryRead/WaitMemoryWrite/WaitCacheRewrite depending on whether an M state block was evicted during allocation.
store (Just MESIWaitMemoryRead) memoryAddress cache memory cacheBus pid = (newMESIState, newCache, newMemory, cacheBus) where
    isMemoryBusy = Memory.isBusy memory

    (maybeEvictedBlockState, cacheAfterAllocate) = case isMemoryBusy of
        True    -> (Nothing, cache) -- Memory is still busy, no allocation is done on this cycle
        False   -> Cache.busAllocate E memoryAddress cache -- Memory is no longer busy, do allocation on local cache in E state

    newMESIState = case isMemoryBusy of
        True    -> MESIWaitMemoryRead -- Memory is still busy, wait for it in WaitMemoryRead state
        False   -> case maybeEvictedBlockState of -- Memory is no longer busy, next state depends on whether an M state block was evicted during allocation
            Just M      -> MESIWaitMemoryWrite -- An M state block was evicted during allocation, issue a memory write and go to WaitMemoryWrite state
            _           -> MESIWaitCacheRewrite -- No M state block was evicted during allocation, issue a cache write and go to WaitCacheRewrite state

    newCache = case isMemoryBusy of
        True    -> cache -- Memory is still busy, no cache operation issued this cycle
        False   -> case maybeEvictedBlockState of -- Memory is no longer busy, cache write might be issued depending on whether M state block was evicted
            Just M      -> cacheAfterAllocate -- An M state block was evicted during allocation, no cache write issued yet
            _           -> Cache.issueWrite memoryAddress cacheAfterAllocate -- No M state block was evicted during allocation, issue a cache write

    newMemory = case isMemoryBusy of
        True    -> memory -- Memory is still busy, no memory operation issued this cycle
        False   -> case maybeEvictedBlockState of -- Memory is no longer busy, memory write might be issued depending on whether M state block was evicted
            Just M      -> Memory.issueWrite memory -- An M state block was evicted during allocation, issue a memory write
            _           -> memory -- No M state block was evicted during allocation, no memory operation required

-- Store on WaitMemoryWrite, this state could only be reached if an M state block was evicted during allocation.
-- Could proceed to WaitMemoryWrite/WaitCacheRewrite depending on whether the memory is still busy.
store (Just MESIWaitMemoryWrite) memoryAddress cache memory cacheBus pid = (newMESIState, newCache, memory, cacheBus) where
    isMemoryBusy = Memory.isBusy memory

    newMESIState = case isMemoryBusy of
        True    -> MESIWaitMemoryWrite -- Memory is still busy, wait for it in WaitMemoryWrite state
        False   -> MESIWaitCacheRewrite -- Memory is no longer busy, immediately issue a cache write and go to WaitCacheRewrite state

    newCache = case isMemoryBusy of
        True    -> cache -- Memory is still busy, no cache write issued on this cycle
        False   -> Cache.issueWrite memoryAddress cache -- Memory is no longer busy, issue a cache write

-- Store on WaitCacheRewrite, this state could only be reached on a write miss, requiring a fetch from the memory.
-- Could proceed to Done/WaitCacheRewrite depending on whether the cache is still busy.
store (Just MESIWaitCacheRewrite) memoryAddress cache memory cacheBus pid = (newMESIState, newCache, memory, newCacheBus) where
    cacheHit = Cache.isCacheHit cache

    newMESIState = case cacheHit of
        Just True   -> MESIDone -- Cache rewrite was a hit, store is done, go to Done state
        Just False  -> error "Cache miss on rewrite"
        Nothing     -> MESIWaitCacheRewrite -- Cache rewrite has not finished, wait in WaitCacheRewrite state

    newCache = case cacheHit of
        Just True   -> Cache.commitWrite memoryAddress cache -- Cache rewrite has finished and was a hit, commit write
        Just False  -> error "Cache miss on rewrite"
        Nothing     -> cache -- Cache rewrite has not finished, do not commit write

    newCacheBus = case cacheHit of
        Just True   -> Bus.release cacheBus -- Cache rewrite was a hit, store is done, release the cache bus
        Just False  -> error "Cache miss on rewrite"
        Nothing     -> cacheBus -- Cache rewrite has not finished, do not release cache bus this cycle

store (Just MESIWaitCacheRead) _ _ _ _ _ = error "WaitCacheRead state on MESIProtocol.store"
store (Just MESIDone) _ _ _ _ _ = error "Done state on MESIProtocol.store"
