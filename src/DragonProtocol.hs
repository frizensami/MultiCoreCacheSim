module DragonProtocol (DragonState (..), load, store) where

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

data DragonState = DragonWaitCacheRead | DragonWaitCacheWrite
    | DragonWaitBusTr
    | DragonWaitMemoryRead | DragonWaitMemoryWrite
    | DragonWaitCacheRewrite | DragonDone deriving (Show)

instance ProtocolState DragonState where
    isDone DragonDone   = True
    isDone _            = False

load :: Maybe DragonState -> MemoryAddress -> Cache -> Memory -> CacheBus -> (DragonState, Cache, Memory, CacheBus)
load Nothing memoryAddress cache memory cacheBus = (newDragonState, newCache, memory, cacheBus) where
    newDragonState = DragonWaitCacheRead
    newCache = Cache.issueRead memoryAddress cache

load (Just DragonWaitCacheRead) memoryAddress cache memory cacheBus = (newDragonState, newCache, newMemory, newCacheBus) where
    cacheHit = Cache.isCacheHit cache

    maybeAcquiredCacheBus = case cacheHit of
        Just True   -> Nothing -- Cache hit, no need to issue bus transaction
        Just False  -> Bus.acquire (DragonBusRd memoryAddress) cacheBus -- Cache miss, try to acquire bus
        Nothing     -> Nothing -- Cache is still busy, no need to issue bus transaction on this cycle

    newDragonState = case cacheHit of
        Just True   -> DragonDone -- Cache hit, go to Done state
        Just False  -> case maybeAcquiredCacheBus of -- Cache miss, next state depends on whether the bus is acquired
            Nothing                 -> DragonWaitCacheRead -- Bus not acquired, keep trying to acquire on WaitCacheRead state
            Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, next state depends on whether the bus is now busy
                True    -> DragonWaitBusTr -- Bus is now busy, wait for it on WaitBusTr state
                False   -> DragonWaitMemoryRead -- Instant bus transaction (no one has a copy), immediately issue a memory read and go to WaitMemoryRead
        Nothing     -> DragonWaitCacheRead -- Cache is still busy, wait for it in WaitCacheRead state

    newCache = case cacheHit of
        Just True   -> Cache.commitRead memoryAddress cache -- Cache hit, do a commit read
        Just False  -> cache -- Cache miss, no commit read required as allocation will be done
        Nothing     -> cache -- Cache miss, no commit read required as allocation will be done

    newMemory = case cacheHit of
        Just True   -> memory -- Cache hit, no memory operation issued
        Just False  -> case maybeAcquiredCacheBus of
            Nothing                 -> memory -- Bus not acquired, keep trying to acquire bus, no memory operation issued
            Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of
                True    -> memory -- Bus is busy, no memory opereation issued on this cycle
                False   -> Memory.issueRead memory -- Bus is not busy (no one has copy), immediately issue a memory read
        Nothing     -> memory -- Cache is still busy, no memory operation issued on this cycle

    newCacheBus = fromMaybe cacheBus maybeAcquiredCacheBus -- Return the acquired cache bus if it is acquired

load (Just DragonWaitBusTr) memoryAddress cache memory cacheBus = (newDragonState, newCache, newMemory, newCacheBus) where
    isBusBusy = Bus.isBusy cacheBus

    (maybeEvictedBlockState, newCache) = case isBusBusy of
        True    -> (Nothing, cache) -- Bus is still busy, no allocation on this cycle
        False   -> Cache.busAllocate SC memoryAddress cache -- Bus is no longer busy, do an allocation on SM state (shared as WaitBusTr indicates forwarding)

    newDragonState = case isBusBusy of
        True    -> DragonWaitBusTr -- Bus is still busy, wait for it in WaitBusTr state
        False   -> case maybeEvictedBlockState of
            Just M  -> DragonWaitMemoryWrite -- An M cache block was evicted during allocation, issue a memory write and go to WaitMemoryWrite state
            Just SM -> DragonWaitMemoryWrite -- An SM cache block was evicted during allocation, issue a memory write and go to WaitMemoryWrite state
            _       -> DragonDone -- No M/SM cache block was evicted during allocation, go to Done state as no more operations are required

    newMemory = case isBusBusy of
        True    -> memory -- Bus is still busy, no memory operation issued on this cycle
        False   -> case maybeEvictedBlockState of
            Just M  -> Memory.issueWrite memory -- An M cache block was evicted during allocation, issue a memory write
            Just SM -> Memory.issueWrite memory -- An SM cache block was evicted during allocation, issue a memory write
            _       -> memory -- No M/SM cache block was evicted, no memory operation issued

    newCacheBus = case isBusBusy of
        True    -> cacheBus -- Bus is still busy, do not release bus
        False   -> case maybeEvictedBlockState of
            Just M  -> cacheBus -- An M cache block was evicted during allocation, do not release bus as a memory write still needs to be done
            Just SM -> cacheBus -- An SM cache block was evicted during allocation, do not release bus as a memory write still needs to be done
            _       -> Bus.release cacheBus -- No M/SM cache block was evicted, Done state reached, release the bus

load (Just DragonWaitMemoryRead) memoryAddress cache memory cacheBus = (newDragonState, newCache, newMemory, newCacheBus) where
    isMemoryBusy = Memory.isBusy memory

    (maybeEvictedBlockState, newCache) = case isMemoryBusy of
        True    -> (Nothing, cache) -- Memory is still busy, no allocation on this cycle
        False   -> Cache.busAllocate E memoryAddress cache -- Memory is no longer busy, do an allocation on E state (exclusive as memory read was required)

    newDragonState = case isMemoryBusy of
        True    -> DragonWaitMemoryRead -- Memory is still busy, wait for it in WaitMemoryRead state
        False   -> case maybeEvictedBlockState of
            Just M  -> DragonWaitMemoryWrite -- An M cache block was evicted during allocation, issue a memory write and go to WaitMemoryWrite state
            Just SM -> DragonWaitMemoryWrite -- An SM cache block was evicted during allocation, issue a memory write and go to WaitMemoryWrite state
            _       -> DragonDone -- No M/SM cache block was evicted, go to Done state as no more operations are required

    newMemory = case isMemoryBusy of
        True    -> memory -- Memory is still busy, no memory operation issued on this cycle
        False   -> case maybeEvictedBlockState of
            Just M  -> Memory.issueWrite memory -- An M cache block was evicted during allocation, issue a memory write
            Just SM -> Memory.issueWrite memory -- An SM cache block was evicted during allocation, issue a memory write
            _       -> memory -- No M/SM cache block was evicted, no memory operation needs to be issued

    newCacheBus = case isMemoryBusy of
        True    -> cacheBus -- Memory is still busy, do not release bus
        False   -> case maybeEvictedBlockState of
            Just M  -> cacheBus -- An M cache block was evicted during allocation, do not release bus as a memory write still needs to be done
            Just SM -> cacheBus -- An SM cache block was evicted during allocation, do not release bus as a memory write still needs to be done
            _       -> Bus.release cacheBus -- No M/SM cache block was evicted, Done state reached, release the bus

load (Just DragonWaitMemoryWrite) memoryAddress cache memory cacheBus = (newDragonState, cache, memory, newCacheBus) where
    isMemoryBusy = Memory.isBusy memory

    newDragonState = case isMemoryBusy of
        True    -> DragonWaitMemoryWrite -- Memory is still busy, wait for it in WaitMemoryWrite state
        False   -> DragonDone -- Memory is no longer busy, go to Done state as no more operations are required

    newCacheBus = case isMemoryBusy of
        True    -> cacheBus -- Memory is still busy, do not release bus
        False   -> Bus.release cacheBus -- Memory is no longer busy, Done state reached, release the bus

load (Just DragonWaitCacheWrite) _ _ _ _ = error "WaitCacheWrite state on DragonProtocol.load"
load (Just DragonWaitCacheRewrite) _ _ _ _ = error "WaitCacheRewrite state on DragonProtocol.load"
load (Just DragonDone) _ _ _ _ = error "Done state on DragonProtocol.load"

store :: Maybe DragonState -> MemoryAddress -> Cache -> Memory -> CacheBus -> (DragonState, Cache, Memory, CacheBus)
store Nothing memoryAddress cache memory cacheBus = (DragonWaitCacheWrite, cache, memory, cacheBus)

store (Just DragonWaitCacheWrite) memoryAddress cache memory cacheBus = (DragonWaitCacheWrite, cache, memory, cacheBus)

store (Just DragonWaitCacheRewrite) memoryAddress cache memory cacheBus = (DragonWaitCacheRewrite, cache, memory, cacheBus)

store (Just DragonWaitBusTr) memoryAddress cache memory cacheBus = (DragonWaitBusTr, cache, memory, cacheBus)

store (Just DragonWaitMemoryRead) memoryAddress cache memory cacheBus = (DragonWaitMemoryRead, cache, memory, cacheBus)

store (Just DragonWaitMemoryWrite) memoryAddress cache memory cacheBus = (DragonWaitMemoryWrite, cache, memory, cacheBus)

store (Just DragonWaitCacheRead) _ _ _ _ = error "WaitCacheRead state on DragonProtocol.store"
store (Just DragonDone) _ _ _ _ = error "Done state on DragonProtocol.store"
