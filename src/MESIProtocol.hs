module MESIProtocol (MESIState (..), MESIProtocol, load) where

import Bus (CacheBus, BusTr (..))
import qualified Bus
import Cache (Cache)
import qualified Cache
import CacheBlock (BlockState (..))
import Data.Maybe
import Definitions
import Memory (Memory)
import qualified Memory
import Prelude hiding (read)

data MESIState = MESIWaitCacheRead | MESIWaitCacheWrite
    | MESIIssueBusTr BusTr | MESIWaitBusTr
    | MESIWaitMemoryRead | MESIWaitMemoryWrite
    | MESIDone

data MESIProtocol = MESIProtocol MESIState

-- |Loads a memory address to the processor while adhering to the MESI coherence protocol.
--  Returns the renewed MESIProtocol containing the protocol state, cache, memory, and cache bus.
load :: Maybe MESIProtocol -> MemoryAddress -> Cache -> Memory -> CacheBus -> (MESIProtocol, Cache, Memory, CacheBus)
load Nothing memoryAddress cache memory cacheBus = (MESIProtocol newMESIState, newCache, memory, cacheBus) where
    newMESIState = MESIWaitCacheRead
    newCache = Cache.issueRead memoryAddress cache

-- Load on WaitCacheRead. Could proceed to IssueBusTr/WaitBusTr/WaitMemoryRead/Done depending on the cache/bus states.
load (Just (MESIProtocol MESIWaitCacheRead)) memoryAddress cache memory cacheBus = (MESIProtocol newMESIState, cache, newMemory, newCacheBus) where
    cacheHit = Cache.isCacheHit cache

    maybeAcquiredCacheBus = case cacheHit of
        Just True   -> Nothing -- Cache hit, no need to issue bus transaction
        Just False  -> Bus.acquire (MESIBusRd memoryAddress) cacheBus -- Cache miss, try to issue a bus transaction through acquire
        Nothing     -> Nothing -- Cache read not finished yet, no need to issue bus transaction on this cycle

    newMESIState = case cacheHit of
        Just True   -> MESIDone -- Cache hit, load operation is done
        Just False  -> mesiStateAfterMaybeAcquiredBus -- Cache miss, next state depends on the bus state after acquire attempt
        Nothing     -> MESIWaitCacheRead -- Cache read not finished yet, wait until it is finished
        where
            mesiStateAfterMaybeAcquiredBus = case maybeAcquiredCacheBus of
                Nothing                 -> MESIIssueBusTr $ MESIBusRd memoryAddress -- Bus can't be acquired, keep trying to acquire
                Just acquiredCacheBus   -> mesiStateAfterAcquiredBus acquiredCacheBus -- Bus acquired, next state depends on whether bus transaction is instant
                where
                    mesiStateAfterAcquiredBus x
                        | Bus.isBusy x  = MESIWaitBusTr -- Bus is busy, wait for bus transaction to finish
                        | otherwise     = MESIWaitMemoryRead -- Bus transaction is instant, do a memory read and go to memory read state

    newMemory
        | cacheHit == Just True     = memory -- Cache hit, no memory operation needed
        | cacheHit == Just False    = memoryAfterMaybeAcquiredBus -- Cache miss, memory might be modified depending on the bus state after acquire attempt
        | otherwise                 = memory -- Cache read not finished yet, no memory operation needed
        where
            memoryAfterMaybeAcquiredBus = case maybeAcquiredCacheBus of
                Nothing                 -> memory -- Bus can't be acquired, no memory modification while trying to acquire bus
                Just acquiredCacheBus   -> memoryAfterAcquiredBus acquiredCacheBus -- Bus acquired, memory might be modified depending on whether bus transaction is instant
                where
                    memoryAfterAcquiredBus x
                        | Bus.isBusy x  = memory -- Bus is busy, no memory modification until bus transaction is finished
                        | otherwise     = Memory.issueRead memory -- Bus transaction is instant, do a memory read

    newCacheBus
        | cacheHit == Just True     = Bus.release cacheBus -- Cache hit, load operation done, release cache bus
        | cacheHit == Just False    = fromMaybe cacheBus maybeAcquiredCacheBus -- Cache miss, returns the acquired cache bus if the bus it is acquired
        | otherwise                 = cacheBus -- Cache read not finished yet, no change to cache bus

-- Load on IssueBusTr, this state could only be reached if bus was busy during after cache read is done. Could proceed to IssueBusTr/WaitBusTr/WaitMemoryRead
-- depending on the bus state.
load (Just (MESIProtocol (MESIIssueBusTr busTr))) memoryAddress cache memory cacheBus = (MESIProtocol newMESIState, cache, newMemory, newCacheBus) where
    maybeAcquiredCacheBus = Bus.acquire busTr cacheBus

    newMESIState = case maybeAcquiredCacheBus of
        Nothing                   -> MESIIssueBusTr busTr -- Bus can't be acquired, keep trying to acquire bus
        Just acquiredCacheBus     -> mesiStateAfterAcquiredBus acquiredCacheBus -- Bus acquired, next state depends on whether the bus transaction is instant
        where
            mesiStateAfterAcquiredBus x
                | Bus.isBusy x  = MESIWaitBusTr -- Bus is busy (non-instant transaction), wait for it to finish
                | otherwise     = MESIWaitMemoryRead -- Bus transaction is instant, do a memory read and go to memory read state

    newMemory = case maybeAcquiredCacheBus of
        Nothing                 -> memory -- Bus can't be acquired, no change to memory while waiting for bus acquisition
        Just acquiredCacheBus   -> memoryAfterAcquiredBus acquiredCacheBus -- Bus acquired, memory operations might be issued depending on whether the bus transaction is instant
        where
            memoryAfterAcquiredBus x
                | Bus.isBusy x  = memory -- Bus is busy (non-instant transaction), no change to memory during bus busy state
                | otherwise     = Memory.issueRead memory -- Bus transaction is instant, do a memory read

    newCacheBus = case maybeAcquiredCacheBus of
        Nothing               -> cacheBus -- Bus can't be acquired, no change to bus
        Just acquiredCacheBus -> acquiredCacheBus -- Bus acquired, update bus to the newly acquired bus

-- Load on WaitBusTr, this state could only be reached if the bus transaction issued was not instant (a cache has the address cached on M state, for example).
-- Could proceed to WaitBusTr/WaitMemoryRead depending on the bus state.
load (Just (MESIProtocol MESIWaitBusTr)) memoryAddress cache memory cacheBus = (MESIProtocol newMESIState, cache, newMemory, cacheBus) where
    isBusBusy = Bus.isBusy cacheBus

    newMESIState
        | isBusBusy = MESIWaitBusTr -- Bus is still busy, wait for it in WaitBusTr state.
        | otherwise = MESIWaitMemoryRead -- Bus is no longer busy, do a memory read and go to memory read state

    newMemory
        | isBusBusy = memory -- Bus is still busy, no changes to memory while waiting for bus.
        | otherwise = Memory.issueRead memory -- Bus is no longer busy, do a memory read.

-- Load on WaitMemoryRead. Could proceed to WaitMemoryRead/WaitMemoryWrite/Done depending on the memory state and cache allocation result.
load (Just (MESIProtocol MESIWaitMemoryRead)) memoryAddress cache memory cacheBus = (MESIProtocol newMESIState, newCache, newMemory, newCacheBus) where
    isMemoryBusy = Memory.isBusy memory

    (maybeEvictedBlockState, newCache)
        | isMemoryBusy  = (Nothing, cache) -- Memory is still busy, no allocation done on this cycle
        | otherwise     = Cache.busAllocate S memoryAddress cache -- Memory is no longer busy, do allocation on local cache

    newMESIState
        | isMemoryBusy  = MESIWaitMemoryRead -- Memory is still busy, wait for it to complete
        | otherwise     = mesiStateAfterAllocate -- Memory is no longer busy, next state depends on whether allocation results in eviction of M cache block
        where
            mesiStateAfterAllocate = case maybeEvictedBlockState of
                Just M      -> MESIWaitMemoryWrite -- An M cache block was evicted, needs to be written back to memory, wait for this memory write
                otherwise   -> MESIDone -- No M cache block was evicted, go to done state

    newMemory
        | isMemoryBusy  = memory -- Memory is still busy, do not issue any memory operation
        | otherwise     = memoryAfterAllocate -- Memory is no longer busy, a memory write might need to be done due to M block eviction
        where
            memoryAfterAllocate = case maybeEvictedBlockState of
                Just M      -> Memory.issueWrite memory -- An M cache block was evicted, needs to be written back to memory
                otherwise   -> memory -- No M cache block was evicted, no more memory operations issued

    newCacheBus
        | isMemoryBusy  = cacheBus -- Memory is still busy, no changes to cache bus
        | otherwise     = cacheBusAfterAllocate -- Memory is no longer busy, cache bus might need to be released depending on whether Done state is reached.
        where
            cacheBusAfterAllocate = case maybeEvictedBlockState of
                Just M      -> cacheBus -- An M cache block was evicted, Done state not yet reached, don't release the cache bus
                otherwise   -> Bus.release cacheBus -- No M cache block was evicted, Done state reached, release the cache bus

-- Load on WaitMemoryWrite, this state could only be reached if an M state cache block was evicted during cache allocation.
-- Could proceed to WaitMemoryWrite/Done state depending on the memory state.
load (Just (MESIProtocol MESIWaitMemoryWrite)) memoryAddress cache memory cacheBus = (MESIProtocol newMESIState, cache, memory, newCacheBus) where
    isMemoryBusy = Memory.isBusy memory

    newMESIState
        | isMemoryBusy  = MESIWaitMemoryWrite -- Memory write of evicted block is still running, wait until memory write is complete
        | otherwise     = MESIDone -- Memory write of evicted block is complete, go to done state

    newCacheBus
        | isMemoryBusy  = cacheBus -- Memory write of evicted block is still running, no changes to cache bus on this cycle
        | otherwise     = Bus.release cacheBus -- Memory write of evicted block is complete, Done state reached, release the cache bus
