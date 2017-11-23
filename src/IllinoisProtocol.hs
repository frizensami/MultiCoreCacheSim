module IllinoisProtocol (IllinoisState (..), load, store) where

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

data IllinoisState = IllinoisWaitCacheRead | IllinoisWaitCacheWrite
    | IllinoisWaitBusTr
    | IllinoisWaitMemoryRead | IllinoisWaitMemoryWrite
    | IllinoisWaitCacheRewrite | IllinoisDone deriving (Show)

instance ProtocolState IllinoisState where
    isDone IllinoisDone = True
    isDone _        = False

-- |Loads a memory address to the processor while adhering to the Illinois coherence protocol.
--  Returns the renewed IllinoisState, cache, memory, and cache bus.
load :: Maybe IllinoisState -> MemoryAddress -> Cache -> Memory -> CacheBus -> Int -> (IllinoisState, Cache, Memory, CacheBus)
load Nothing memoryAddress cache memory cacheBus pid = (newIllinoisState, newCache, memory, cacheBus) where
    newIllinoisState = IllinoisWaitCacheRead
    newCache = Cache.issueRead memoryAddress cache

-- Load on WaitCacheRead.
-- Could proceed to IssueBusTr/WaitBusTr/WaitMemoryRead/Done/WaitCacheRead depending on the cache/bus states.
load (Just IllinoisWaitCacheRead) memoryAddress cache memory cacheBus pid = (newIllinoisState, newCache, newMemory, newCacheBus) where
    cacheHit = Cache.isCacheHit cache

    maybeAcquiredCacheBus = case cacheHit of
        Just True   -> Nothing -- Cache hit, no need to issue bus transaction
        Just False  -> Bus.acquire (IllinoisBusRd memoryAddress) cacheBus -- Cache miss, try to issue a bus transaction through acquire
        Nothing     -> Nothing -- Cache read not finished yet, no need to issue bus transaction on this cycle

    newIllinoisState = case cacheHit of
        Just True   -> IllinoisDone -- Cache hit, load operation is done
        Just False  -> case maybeAcquiredCacheBus of -- Cache miss, next state depends on the bus state after acquire attempt
            Nothing                 -> IllinoisWaitCacheRead -- Bus can't be acquired, keep trying to acquire on WaitCacheRead state
            Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of
                True    -> IllinoisWaitBusTr -- Bus is busy, wait for bus transaction to finish
                False   -> IllinoisWaitMemoryRead -- Bus transaction is instant, do a memory read and go to memory read state
        Nothing     -> IllinoisWaitCacheRead -- Cache read not finished yet, wait until it is finished

    newCache = case cacheHit of
        Just True   -> Cache.commitRead memoryAddress cache -- Cache hit, do a commit read
        Just False  -> cache -- Cache miss, no changes to cache
        Nothing     -> cache -- Cache read not finished yet, no changes to cache

    newMemory = case cacheHit of
        Just True   -> memory -- Cache hit, no memory operation needed
        Just False  -> case maybeAcquiredCacheBus of -- Cache miss, memory might be modified depending on the bus state after acquire attempt
            Nothing                 -> memory -- Bus can't be acquired, no memory modification while trying to acquire bus
            Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, memory might be modified depending on whether bus transaction is instant
                True    -> memory -- Bus is busy, no memory modification until bus transaction is finished
                False   -> Memory.issueRead memory -- Bus transaction is instant, do a memory read
        Nothing     -> memory -- Cache read not finished yet, no memory operation needed

    newCacheBus = fromMaybe cacheBus maybeAcquiredCacheBus -- Returns the acquired cache bus if the bus is acquired

-- Load on WaitBusTr, this state could only be reached if the bus transaction issued was not instant (a cache has the address cached on M state, for example).
-- Could proceed to WaitBusTr/WaitMemoryRead depending on the bus state.
load (Just IllinoisWaitBusTr) memoryAddress cache memory cacheBus pid = (newIllinoisState, newCache, newMemory, newCacheBus) where
    isBusBusy = Bus.isBusy cacheBus

    (maybeEvictedBlockState, newCache) = case isBusBusy of
        True    -> (Nothing, cache) -- Bus is still busy, do not allocate on this cycle
        False   -> Cache.busAllocate S memoryAddress cache -- Bus is no longer busy, do allocation on S state (bus busy -> cache-to-cache sharing)

    newIllinoisState = case isBusBusy of
        True    -> IllinoisWaitBusTr -- Bus is still busy, wait for it in WaitBusTr state.
        False   -> case maybeEvictedBlockState of -- Bus is no longer busy, next state depends on whether an M block was evicted on allocation
            Just M  -> IllinoisWaitMemoryWrite -- An M block was evicted on allocation, issue a memory write and go to WaitMemoryWrite state
            _       -> IllinoisDone -- No M block was evicted on allocation, go to Done state as nothing else needs to be done

    newMemory = case isBusBusy of
        True    -> memory -- Bus is still busy, no changes to memory while waiting for bus.
        False   -> case maybeEvictedBlockState of -- Bus is no longer busy, might need to issue memory write depending on whether M block was evicted
            Just M  -> Memory.issueWrite memory -- An M block was evicted on allocation, issue a memory write
            _       -> memory -- No M block was evicted on allocation, Done state reached, no change to memory

    newCacheBus = case isBusBusy of
        True    -> cacheBus -- Bus is still busy, no change to cache bus
        False   -> case maybeEvictedBlockState of -- Bus is no longer busy, might need to release bus depending on whether M block was evicted
            Just M  -> cacheBus -- An M block was evicted on allocation, do not release bus here as memory write still needs to be done
            _       -> Bus.release cacheBus -- No M block was evicted on allocation, Done state reached, release bus

-- Load on WaitMemoryRead.
-- Could proceed to WaitMemoryRead/WaitMemoryWrite/Done depending on the memory state and cache allocation result.
load (Just IllinoisWaitMemoryRead) memoryAddress cache memory cacheBus pid = (newIllinoisState, newCache, newMemory, newCacheBus) where
    isMemoryBusy = Memory.isBusy memory

    (maybeEvictedBlockState, newCache) = case isMemoryBusy of
        True    -> (Nothing, cache) -- Memory is still busy, no allocation done on this cycle
        False   -> Cache.busAllocate E memoryAddress cache -- Memory is no longer busy, cache block is not shared, allocate in E state

    newIllinoisState = case isMemoryBusy of
        True    -> IllinoisWaitMemoryRead -- Memory is still busy, wait for it to complete
        False   -> case maybeEvictedBlockState of -- Memory is no longer busy, next state depends on whether allocation results in eviction of M cache block
            Just M      -> IllinoisWaitMemoryWrite -- An M cache block was evicted, needs to be written back to memory, wait for this memory write
            _           -> IllinoisDone -- No M cache block was evicted, go to done state

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
load (Just IllinoisWaitMemoryWrite) memoryAddress cache memory cacheBus pid = (newIllinoisState, cache, memory, newCacheBus) where
    isMemoryBusy = Memory.isBusy memory

    newIllinoisState = case isMemoryBusy of
        True    -> IllinoisWaitMemoryWrite -- Memory write of evicted block is still running, wait until memory write is complete
        False   -> IllinoisDone -- Memory write of evicted block is complete, go to done state

    newCacheBus = case isMemoryBusy of
        True    -> cacheBus -- Memory write of evicted block is still running, no changes to cache bus on this cycle
        False   -> Bus.release cacheBus -- Memory write of evicted block is complete, Done state reached, release the cache bus

load (Just IllinoisWaitCacheWrite) _ _ _ _ _ = error "WaitCacheWrite state on IllinoisProtocol.load"
load (Just IllinoisWaitCacheRewrite) _ _ _ _ _ = error "WaitCacheRewrite state on IllinoisProtocol.load"
load (Just IllinoisDone) _ _ _ _ _ = error "Done state on IllinoisProtocol.load"

-- |Stores a memory address from the processor while adhering to the Illinois coherence protocol.
--  Returns the renewed IllinoisState, cache, memory, and cache bus.
store :: Maybe IllinoisState -> MemoryAddress -> Cache -> Memory -> CacheBus -> Int -> (IllinoisState, Cache, Memory, CacheBus)
store Nothing memoryAddress cache memory cacheBus pid = (newIllinoisState, newCache, memory, cacheBus) where
    newIllinoisState = IllinoisWaitCacheWrite
    newCache = Cache.issueWrite memoryAddress cache

-- Store on WaitCacheWrite.
-- Could proceed to Done/IssueBusTr/WaitBusTr/WaitMemoryRead/WaitCacheWrite depending on the bus states and whether it is a cache hit.
store (Just IllinoisWaitCacheWrite) memoryAddress cache memory cacheBus pid = (newIllinoisState, newCache, newMemory, newCacheBus) where
    cacheHit = Cache.isCacheHit cache
    blockState = Cache.busGetBlockState memoryAddress cache

    maybeAcquiredCacheBus = case cacheHit of
        Just True   -> case blockState of -- Cache hit, bus transaction might be issued depending on the block state
            Just M      -> Nothing -- Cache hit on M state, no need to acquire the bus
            Just E      -> Nothing -- Cache hit on E state, no need to acquire the bus
            Just S      -> Bus.acquire (IllinoisBusUpg memoryAddress) cacheBus -- Cache hit on S state, issue a IllinoisBusUpg transaction to invalidate other copies
            _           -> Bus.acquire (IllinoisBusRdX memoryAddress) cacheBus -- Cache hit on I state, block changed through bus by other protocol -> IllinoisBusRdX
        Just False  -> Bus.acquire (IllinoisBusRdX memoryAddress) cacheBus -- Cache miss, issue a IllinoisBusRdX transaction to invalidate other copies and read from memory
        Nothing     -> Nothing -- Cache write has not completed, no need to acquire the bus on this cycle

    newIllinoisState = case cacheHit of
        Just True   -> case blockState of -- Cache hit, next state depends on the block state
            Just M      -> IllinoisDone -- Cache hit on M state, immediately go to Done state
            Just E      -> IllinoisDone -- Cache hit on E state, immediately go to Done state
            Just S      -> case maybeAcquiredCacheBus of -- Cache hit on S state, next state depends on whether bus is acquired
                Nothing                 -> IllinoisWaitCacheWrite -- Bus not acquired, keep trying to acquire on WaitCacheWrite state
                Just acquiredCacheBus   -> IllinoisDone -- Bus acquired, invalidation is instant, go to Done state
            _           -> case maybeAcquiredCacheBus of -- Cache hit on I state, block changed through bus, next state depends on whether the bus is acquired
                Nothing                 -> IllinoisWaitCacheWrite -- Bus not acquired, keep trying to acquire on WaitCacheWrite state
                Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, next state depends on whether bus transaction is instant
                    True    -> IllinoisWaitBusTr -- Bus is busy (transaction not instant), go to WaitBusTr state 
                    False   -> IllinoisWaitMemoryRead -- Bus transaction is instant, issue memory read and go to WaitMemoryRead state
        Just False  -> case maybeAcquiredCacheBus of -- Cache miss, next state depends on whether bus is acquired
            Nothing                 -> IllinoisWaitCacheWrite -- Bus not acquired, keep trying to acquire on IssueBusTr state
            Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, next state depends on whether bus transaction is instant
                True    -> IllinoisWaitBusTr -- Bus is busy (transaction not instant), go to WaitBusTr state
                False   -> IllinoisWaitMemoryRead -- Bus transaction is instant, issue memory read and go to WaitMemoryRead state
        Nothing     -> IllinoisWaitCacheWrite

    newCache = case cacheHit of
        Just True   -> case blockState of
            Just M      -> Cache.commitWrite memoryAddress cache -- Cache hit on M block, commit write
            Just E      -> Cache.commitWrite memoryAddress cache -- Cache hit on E block, commit write
            Just S      -> case maybeAcquiredCacheBus of
                Nothing                 -> cache -- Bus can't be acquired, do not commit write on this cycle to maintain original cache state
                Just acquiredCacheBus   -> Cache.commitWrite memoryAddress cache -- Bus acquired, commit write is now allowed
            _           -> cache -- Cache hit on I state, block changed through bus, no changes to cache
        Just False  -> cache -- Cache miss, no changes to cache
        Nothing     -> cache -- Cache is still busy, no changes to cache

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
            Just E      -> cacheBus -- Cache hit on E state, no bus transaction generated, return the same bus
            Just S      -> case maybeAcquiredCacheBus of -- Cache hit on S state, new bus depends on whether the bus is acquired successfully
                Nothing                 -> cacheBus -- Bus has not been acquired, return the same bus
                Just acquiredCacheBus   -> Bus.release acquiredCacheBus -- Bus has been acquired, will not be busy (only invalidate), release acquired bus
            _           -> fromMaybe cacheBus maybeAcquiredCacheBus -- Cache hit on I state, block changed through bus, returns the acquired cache bus if any
        Just False  -> fromMaybe cacheBus maybeAcquiredCacheBus -- Cache miss, returns the acquired cache bus if the bus is acquired
        Nothing     -> cacheBus -- Cache read not finished yet, no change to cache bus

-- Store on WaitBusTr, this state could only be reached if the issued bus transaction was not instant.
-- Could proceed to WaitBusTr/WaitMemoryRead depending on whether the bus is still busy.
store (Just IllinoisWaitBusTr) memoryAddress cache memory cacheBus pid = (newIllinoisState, newCache, newMemory, cacheBus) where
    isBusBusy = Bus.isBusy cacheBus

    (maybeEvictedBlockState, cacheAfterAllocate) = case isBusBusy of
        True    -> (Nothing, cache) -- Bus is still busy, no allocation on this cycle
        False   -> Cache.busAllocate E memoryAddress cache

    newIllinoisState = case isBusBusy of
        True    -> IllinoisWaitBusTr -- Bus is busy, wait for the bus completion in WaitBusTr state
        False   -> case maybeEvictedBlockState of -- Bus is no longer busy, next state depends on whether an M block was evicted on allocation
            Just M  -> IllinoisWaitMemoryWrite -- An M block was evicted on allocation, issue a memory write and go to WaitMemoryWrite state
            _       -> IllinoisWaitCacheRewrite -- No M block was evicted on allocation, proceed to issue cache write and go to WaitCacheRewrite state

    newCache = case isBusBusy of
        True    -> cache -- Bus is busy, no changes to cache on this cycle
        False   -> case maybeEvictedBlockState of -- Bus is no longer busy, cache write might need to be issued depending on whether an M block was evicted
            Just M  -> cacheAfterAllocate -- An M block was evicted on allocation, cache write not issued here as memory write still needs to be done
            _       -> Cache.issueWrite memoryAddress cacheAfterAllocate -- No M block was evicted on allocation, immediately issue a cache write

    newMemory = case isBusBusy of
        True    -> memory -- Bus is busy, no memory read issued this cycle
        False   -> case maybeEvictedBlockState of -- Bus is no longer busy, might need to issue memory read depending on whether an M block was evicted
            Just M  -> Memory.issueWrite memory -- An M block was evicted on allocation, issue a memory write
            _       -> memory -- No M block was evicted on allocation, no need to issue a memory write

-- Store on WaitMemoryRead.
-- Could proceed to WaitMemoryRead/WaitMemoryWrite/WaitCacheRewrite depending on whether an M state block was evicted during allocation.
store (Just IllinoisWaitMemoryRead) memoryAddress cache memory cacheBus pid = (newIllinoisState, newCache, newMemory, cacheBus) where
    isMemoryBusy = Memory.isBusy memory

    (maybeEvictedBlockState, cacheAfterAllocate) = case isMemoryBusy of
        True    -> (Nothing, cache) -- Memory is still busy, no allocation is done on this cycle
        False   -> Cache.busAllocate E memoryAddress cache -- Memory is no longer busy, do allocation on local cache in E state

    newIllinoisState = case isMemoryBusy of
        True    -> IllinoisWaitMemoryRead -- Memory is still busy, wait for it in WaitMemoryRead state
        False   -> case maybeEvictedBlockState of -- Memory is no longer busy, next state depends on whether an M state block was evicted during allocation
            Just M      -> IllinoisWaitMemoryWrite -- An M state block was evicted during allocation, issue a memory write and go to WaitMemoryWrite state
            _           -> IllinoisWaitCacheRewrite -- No M state block was evicted during allocation, issue a cache write and go to WaitCacheRewrite state

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
store (Just IllinoisWaitMemoryWrite) memoryAddress cache memory cacheBus pid = (newIllinoisState, newCache, memory, cacheBus) where
    isMemoryBusy = Memory.isBusy memory

    newIllinoisState = case isMemoryBusy of
        True    -> IllinoisWaitMemoryWrite -- Memory is still busy, wait for it in WaitMemoryWrite state
        False   -> IllinoisWaitCacheRewrite -- Memory is no longer busy, immediately issue a cache write and go to WaitCacheRewrite state

    newCache = case isMemoryBusy of
        True    -> cache -- Memory is still busy, no cache write issued on this cycle
        False   -> Cache.issueWrite memoryAddress cache -- Memory is no longer busy, issue a cache write

-- Store on WaitCacheRewrite, this state could only be reached on a write miss, requiring a fetch from the memory.
-- Could proceed to Done/WaitCacheRewrite depending on whether the cache is still busy.
store (Just IllinoisWaitCacheRewrite) memoryAddress cache memory cacheBus pid = (newIllinoisState, newCache, memory, newCacheBus) where
    cacheHit = Cache.isCacheHit cache

    newIllinoisState = case cacheHit of
        Just True   -> IllinoisDone -- Cache rewrite was a hit, store is done, go to Done state
        Just False  -> error "Cache miss on rewrite"
        Nothing     -> IllinoisWaitCacheRewrite -- Cache rewrite has not finished, wait in WaitCacheRewrite state

    newCache = case cacheHit of
        Just True   -> Cache.commitWrite memoryAddress cache -- Cache rewrite has finished and was a hit, commit write
        Just False  -> error "Cache miss on rewrite"
        Nothing     -> cache -- Cache rewrite has not finished, do not commit write

    newCacheBus = case cacheHit of
        Just True   -> Bus.release cacheBus -- Cache rewrite was a hit, store is done, release the cache bus
        Just False  -> error "Cache miss on rewrite"
        Nothing     -> cacheBus -- Cache rewrite has not finished, do not release cache bus this cycle

store (Just IllinoisWaitCacheRead) _ _ _ _ _ = error "WaitCacheRead state on IllinoisProtocol.store"
store (Just IllinoisDone) _ _ _ _ _ = error "Done state on IllinoisProtocol.store"
