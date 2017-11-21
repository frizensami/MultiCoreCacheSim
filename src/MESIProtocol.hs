module MESIProtocol (MESIState (..), load, store) where

import Bus (CacheBus, BusTr (..))
import qualified Bus
import Cache (Cache)
import qualified Cache
import CacheBlock (BlockState (..))
import Data.Maybe
import Definitions
import Memory (Memory)
import qualified Memory
import Protocol

data MESIState = MESIWaitCacheRead | MESIWaitCacheWrite
    | MESIIssueBusTr BusTr | MESIWaitBusTr
    | MESIWaitMemoryRead | MESIWaitMemoryWrite
    | MESIWaitCacheRewrite | MESIDone

instance ProtocolState MESIState where
    isDone MESIDone = True
    isDone _        = False

-- |Loads a memory address to the processor while adhering to the MESI coherence protocol.
--  Returns the renewed MESIState, cache, memory, and cache bus.
load :: Maybe MESIState -> MemoryAddress -> Cache -> Memory -> CacheBus -> (MESIState, Cache, Memory, CacheBus)
load Nothing memoryAddress cache memory cacheBus = (newMESIState, newCache, memory, cacheBus) where
    newMESIState = MESIWaitCacheRead
    newCache = Cache.issueRead memoryAddress cache

-- Load on WaitCacheRead.
-- Could proceed to IssueBusTr/WaitBusTr/WaitMemoryRead/Done/WaitCacheRead depending on the cache/bus states.
load (Just MESIWaitCacheRead) memoryAddress cache memory cacheBus = (newMESIState, cache, newMemory, newCacheBus) where
    cacheHit = Cache.isCacheHit cache

    maybeAcquiredCacheBus = case cacheHit of
        Just True   -> Nothing -- Cache hit, no need to issue bus transaction
        Just False  -> Bus.acquire (MESIBusRd memoryAddress) cacheBus -- Cache miss, try to issue a bus transaction through acquire
        Nothing     -> Nothing -- Cache read not finished yet, no need to issue bus transaction on this cycle

    newMESIState = case cacheHit of
        Just True   -> MESIDone -- Cache hit, load operation is done
        Just False  -> case maybeAcquiredCacheBus of -- Cache miss, next state depends on the bus state after acquire attempt
            Nothing                 -> MESIIssueBusTr $ MESIBusRd memoryAddress -- Bus can't be acquired, keep trying to acquire
            Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of
                True    -> MESIWaitBusTr -- Bus is busy, wait for bus transaction to finish
                False   -> MESIWaitMemoryRead -- Bus transaction is instant, do a memory read and go to memory read state
        Nothing     -> MESIWaitCacheRead -- Cache read not finished yet, wait until it is finished

    newMemory = case cacheHit of
        Just True   -> memory -- Cache hit, no memory operation needed
        Just False  -> case maybeAcquiredCacheBus of -- Cache miss, memory might be modified depending on the bus state after acquire attempt
            Nothing                 -> memory -- Bus can't be acquired, no memory modification while trying to acquire bus
            Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, memory might be modified depending on whether bus transaction is instant
                True    -> memory -- Bus is busy, no memory modification until bus transaction is finished
                False   -> Memory.issueRead memory -- Bus transaction is instant, do a memory read
        Nothing     -> memory -- Cache read not finished yet, no memory operation needed

    newCacheBus = fromMaybe cacheBus maybeAcquiredCacheBus -- Returns the acquired cache bus if the bus is acquired

-- Load on IssueBusTr, this state could only be reached if bus was busy during after cache read is done.
-- Could proceed to IssueBusTr/WaitBusTr/WaitMemoryRead depending on the bus state.
load (Just (MESIIssueBusTr busTr)) memoryAddress cache memory cacheBus = (newMESIState, cache, newMemory, newCacheBus) where
    maybeAcquiredCacheBus = Bus.acquire busTr cacheBus

    newMESIState = case maybeAcquiredCacheBus of
        Nothing                   -> MESIIssueBusTr busTr -- Bus can't be acquired, keep trying to acquire bus
        Just acquiredCacheBus     -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, next state depends on whether the bus transaction is instant
            True    -> MESIWaitBusTr -- Bus is busy (non-instant transaction), wait for it to finish
            False   -> MESIWaitMemoryRead -- Bus transaction is instant, do a memory read and go to memory read state

    newMemory = case maybeAcquiredCacheBus of
        Nothing                 -> memory -- Bus can't be acquired, no change to memory while waiting for bus acquisition
        Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, memory operations might be issued depending on whether the bus transaction is instant
            True    -> memory -- Bus is busy (bus transaction not instant), no changes to memory
            False   -> Memory.issueRead memory -- Bus transaction is instant, immediately issue a memory read

    newCacheBus = fromMaybe cacheBus maybeAcquiredCacheBus -- Returns the acquired cache bus if the bus is acquired

-- Load on WaitBusTr, this state could only be reached if the bus transaction issued was not instant (a cache has the address cached on M state, for example).
-- Could proceed to WaitBusTr/WaitMemoryRead depending on the bus state.
load (Just MESIWaitBusTr) memoryAddress cache memory cacheBus = (newMESIState, cache, newMemory, cacheBus) where
    isBusBusy = Bus.isBusy cacheBus

    newMESIState = case isBusBusy of
        True    -> MESIWaitBusTr -- Bus is still busy, wait for it in WaitBusTr state.
        False   -> MESIWaitMemoryRead -- Bus is no longer busy, do a memory read and go to memory read state

    newMemory = case isBusBusy of
        True    -> memory -- Bus is still busy, no changes to memory while waiting for bus.
        False   -> Memory.issueRead memory -- Bus is no longer busy, do a memory read.

-- Load on WaitMemoryRead.
-- Could proceed to WaitMemoryRead/WaitMemoryWrite/Done depending on the memory state and cache allocation result.
load (Just MESIWaitMemoryRead) memoryAddress cache memory cacheBus = (newMESIState, newCache, newMemory, newCacheBus) where
    isMemoryBusy = Memory.isBusy memory

    (maybeEvictedBlockState, newCache) = case isMemoryBusy of
        True    -> (Nothing, cache) -- Memory is still busy, no allocation done on this cycle
        False   -> case Bus.isShared memoryAddress cacheBus of -- Memory is no longer busy, do allocation on local cache
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
load (Just MESIWaitMemoryWrite) memoryAddress cache memory cacheBus = (newMESIState, cache, memory, newCacheBus) where
    isMemoryBusy = Memory.isBusy memory

    newMESIState = case isMemoryBusy of
        True    -> MESIWaitMemoryWrite -- Memory write of evicted block is still running, wait until memory write is complete
        False   -> MESIDone -- Memory write of evicted block is complete, go to done state

    newCacheBus = case isMemoryBusy of
        True    -> cacheBus -- Memory write of evicted block is still running, no changes to cache bus on this cycle
        False   -> Bus.release cacheBus -- Memory write of evicted block is complete, Done state reached, release the cache bus

-- |Stores a memory address from the processor while adhering to the MESI coherence protocol.
--  Returns the renewed MESIState, cache, memory, and cache bus.
store :: Maybe MESIState -> MemoryAddress -> Cache -> Memory -> CacheBus -> (MESIState, Cache, Memory, CacheBus)
store Nothing memoryAddress cache memory cacheBus = (newMESIState, newCache, memory, cacheBus) where
    newMESIState = MESIWaitCacheWrite
    newCache = Cache.issueWrite memoryAddress cache

-- Store on WaitCacheWrite.
-- Could proceed to Done/IssueBusTr/WaitBusTr/WaitMemoryRead/WaitCacheWrite depending on the bus states and whether it is a cache hit.
store (Just MESIWaitCacheWrite) memoryAddress cache memory cacheBus = (newMESIState, cache, newMemory, newCacheBus) where
    cacheHit = Cache.isCacheHit cache
    blockState = Cache.busGetBlockState memoryAddress cache

    maybeAcquiredCacheBus = case cacheHit of
        Just True   -> case Cache.busGetBlockState memoryAddress cache of -- Cache hit, bus transaction might be issued depending on the block state
            Just M      -> Nothing -- Cache hit on M state, no need to acquire the bus
            Just E      -> error "Cache block still on E state after write is finished"
            Just S      -> Bus.acquire (MESIBusUpg memoryAddress) cacheBus -- Cache hit on S state, issue a MESIBusUpg transaction to invalidate other copies
            _           -> error "Cache hit on I state"
        Just False  -> Bus.acquire (MESIBusRdX memoryAddress) cacheBus -- Cache miss, issue a MESIBusRdX transaction to invalidate other copies and read from memory
        Nothing     -> Nothing -- Cache write has not completed, no need to acquire the bus on this cycle

    newMESIState = case cacheHit of
        Just True   -> case blockState of -- Cache hit, next state depends on the block state
            Just M      -> MESIDone -- Cache hit on M state, immediately go to Done state
            Just E      -> error "Cache block still on E state after write is finished"
            Just S      -> case maybeAcquiredCacheBus of -- Cache hit on S state, next state depends on whether bus is acquired
                Nothing                 -> MESIIssueBusTr $ MESIBusUpg memoryAddress -- Bus not acquired, keep trying to acquire on IssueBusTr state
                Just acquiredCacheBus   -> MESIDone -- Bus acquired, invalidation is instant, go to Done state
            _           -> error "Cache hit on I state"
        Just False  -> case maybeAcquiredCacheBus of -- Cache miss, next state depends on whether bus is acquired
            Nothing                 -> MESIIssueBusTr $ MESIBusRdX memoryAddress -- Bus not acquired, keep trying to acquire on IssueBusTr state
            Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, next state depends on whether bus transaction is instant
                True    -> MESIWaitBusTr -- Bus is busy (transaction not instant), go to WaitBusTr state
                False   -> MESIWaitMemoryRead -- Bus transaction is instant, issue memory read and go to WaitMemoryRead state
        Nothing     -> MESIWaitCacheWrite

    newMemory = case cacheHit of
        Just True   -> memory -- Cache hit, no memory operation needed
        Just False  -> case maybeAcquiredCacheBus of -- Cache miss, might issue a memory read depending on the bus state after acquire attempt
            Nothing                 -> memory -- Bus can't be acquired, no memory operation issued while trying to acquire bus
            Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, memory might be modified depending on whether bus transaction is instant
                True    -> memory -- Bus is busy, no memory operation issued until bus transaction is finished
                False   -> Memory.issueRead memory -- Bus transaction is instant, issue a memory read
        Nothing     -> memory -- Cache read not finished yet, no memory operation needed

    newCacheBus = case cacheHit of
        Just True   -> case blockState of
            Just M      -> cacheBus -- Cache hit on M state, no bus transaction generated, return the same bus
            Just E      -> error "Cache block still on E state after write is finished"
            Just S      -> case maybeAcquiredCacheBus of -- Cache hit on S state, new bus depends on whether the bus is acquired successfully
                Nothing                 -> cacheBus -- Bus has not been acquired, return the same bus
                Just acquiredCacheBus   -> Bus.release acquiredCacheBus -- Bus has been acquired, will not be busy (only invalidate), release acquired bus
            _           -> error "Cache hit on I state"
        Just False  -> fromMaybe cacheBus maybeAcquiredCacheBus -- Cache miss, returns the acquired cache bus if the bus is acquired
        Nothing     -> cacheBus -- Cache read not finished yet, no change to cache bus

-- Store on IssueBusTr, this state could only be reached if bus was busy after cache write was done and a bus transaction is required.
-- Could proceed to IssueBusTr/Done/WaitBusTr/WaitMemoryRead depending on the bus states.
store (Just (MESIIssueBusTr busTr)) memoryAddress cache memory cacheBus = (newMESIState, cache, newMemory, newCacheBus) where
    maybeAcquiredCacheBus = Bus.acquire busTr cacheBus

    newMESIState = case maybeAcquiredCacheBus of
        Nothing                 -> MESIIssueBusTr busTr -- Bus not acquired, keep trying to acquire bus on IssueBusTr state
        Just acquiredCacheBus   -> case busTr of -- Bus acquired, next state depends on the bus transaction and whether it is instant
            MESIBusUpg _    -> MESIDone -- MESIBusUpg transaction is always instant, go to done state as no other operations need to be done
            MESIBusRdX _    -> case Bus.isBusy acquiredCacheBus of -- MESIBusRdX, next state depends on whether the transaction is instant
                True    -> MESIWaitBusTr -- Bus is busy (non-instant transaction), wait for it to finish
                False   -> MESIWaitMemoryRead -- Bus transaction is instant, issue a memory read and go to memory read state
            _           -> error "Unknown bus transaction on MESIProtocol.store"

    newMemory = case maybeAcquiredCacheBus of
        Nothing                 -> memory -- Bus not acquired, no memory operation issued on this cycle
        Just acquiredCacheBus   -> case busTr of -- Bus acquired, memory read might be issued depending on the bus transaction and whether it is instant
            MESIBusUpg _    -> memory -- MESIBusUpg transaction is always instant, no memory read needs to be issued
            MESIBusRdX _    -> case Bus.isBusy acquiredCacheBus of -- MESIBusRdX, next state depends on whether the transaction is instant
                True    -> memory -- Bus is busy (non-instant transaction), no memory read issued on this cycle
                False   -> Memory.issueRead memory -- Bus transaction is instant, issue a memory read and go to memory read state
            _               -> error "Unknown bus transaction on MESIProtocol.store"

    newCacheBus = case maybeAcquiredCacheBus of
        Nothing                 -> cacheBus -- Bus not acquired, no changes to bus on this cycle
        Just acquiredCacheBus   -> case busTr of -- Bus acquired, new cache bus depends on the bus transaction
            MESIBusUpg _    -> Bus.release acquiredCacheBus -- MESIBusUpg transaction is instant and store is finished, immediately release the bus afterwards
            MESIBusRdX _    -> acquiredCacheBus -- MESIBusRdX transaction will always result in an acquired cache bus
            _               -> error "Unknown bus transaction on MESIProtocol.store"

-- Store on WaitBusTr, this state could only be reached if the issued bus transaction was not instant.
-- Could proceed to WaitBusTr/WaitMemoryRead depending on whether the bus is still busy.
store (Just MESIWaitBusTr) memoryAddress cache memory cacheBus = (newMESIState, cache, newMemory, cacheBus) where
    isBusBusy = Bus.isBusy cacheBus

    newMESIState = case isBusBusy of
        True    -> MESIWaitBusTr -- Bus is busy, wait for the bus completion in WaitBusTr state
        False   -> MESIWaitMemoryRead -- Bus is no longer busy, issue memory read and go to WaitMemoryRead state

    newMemory = case isBusBusy of
        True    -> memory -- Bus is busy, no memory read issued this cycle
        False   -> Memory.issueRead memory -- Bus is no longer busy, issue memory read

-- Store on WaitMemoryRead.
-- Could proceed to WaitMemoryRead/WaitMemoryWrite/WaitCacheRewrite depending on whether an M state block was evicted during allocation.
store (Just MESIWaitMemoryRead) memoryAddress cache memory cacheBus = (newMESIState, newCache, newMemory, cacheBus) where
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
store (Just MESIWaitMemoryWrite) memoryAddress cache memory cacheBus = (newMESIState, newCache, memory, cacheBus) where
    isMemoryBusy = Memory.isBusy memory

    newMESIState = case isMemoryBusy of
        True    -> MESIWaitMemoryWrite -- Memory is still busy, wait for it in WaitMemoryWrite state
        False   -> MESIWaitCacheRewrite -- Memory is no longer busy, immediately issue a cache write and go to WaitCacheRewrite state

    newCache = case isMemoryBusy of
        True    -> cache -- Memory is still busy, no cache write issued on this cycle
        False   -> Cache.issueWrite memoryAddress cache -- Memory is no longer busy, issue a cache write

-- Store on WaitCacheRewrite, this state could only be reached on a write miss, requiring a fetch from the memory.
-- Could proceed to Done/WaitCacheRewrite depending on whether the cache is still busy.
store (Just MESIWaitCacheRewrite) memoryAddress cache memory cacheBus = (newMESIState, cache, memory, newCacheBus) where
    cacheHit = Cache.isCacheHit cache

    newMESIState = case cacheHit of
        Just True   -> MESIDone -- Cache rewrite was a hit, store is done, go to Done state
        Just False  -> error "Cache miss on rewrite"
        Nothing     -> MESIWaitCacheRewrite -- Cache rewrite has not finished, wait in WaitCacheRewrite state

    newCacheBus = case cacheHit of
        Just True   -> Bus.release cacheBus -- Cache rewrite was a hit, store is done, release the cache bus
        Just False  -> error "Cache miss on rewrite"
        Nothing     -> cacheBus -- Cache rewrite has not finished, do not release cache bus this cycle
