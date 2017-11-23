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
store Nothing memoryAddress cache memory cacheBus = (newDragonState, newCache, memory, cacheBus) where
    newDragonState = DragonWaitCacheWrite
    newCache = Cache.issueWrite memoryAddress cache

store (Just DragonWaitCacheWrite) memoryAddress cache memory cacheBus = (newDragonState, newCache, newMemory, newCacheBus) where
    isCacheHit = Cache.isCacheHit cache
    blockState = Cache.busGetBlockState memoryAddress cache

    maybeAcquiredCacheBus = case isCacheHit of
        Just True   -> case blockState of -- Cache hit, might need to acquire cache bus depending on the block state
            Just M  -> Nothing -- Cache hit on M state, no need to acquire cache bus
            Just E  -> Nothing -- Cache hit on E state, no need to acquire cache bus
            Just SM -> Bus.acquire (DragonBusUpd memoryAddress) cacheBus -- Cache hit on SM state, issue BusUpd
            Just SC -> Bus.acquire (DragonBusUpd memoryAddress) cacheBus -- Cache hit on SC state, issue BusUpd
            _       -> error "Cache hit on I state in Dragon Protocol"
        Just False  -> Bus.acquire (DragonBusRd memoryAddress) cacheBus -- Cache miss, issue BusRdUpd
        Nothing     -> Nothing -- Cache is still busy, wait for it in WaitCacheWrite state

    newDragonState = case isCacheHit of
        Just True   -> case blockState of -- Cache hit, next state depends on the block state and whether bus was acquired
            Just M  -> DragonDone -- Cache hit on M state, go to Done state as no more operations need to be done
            Just E  -> DragonDone -- Cache hit on E state, go to Done state as no more operations need to be done
            Just SM -> case maybeAcquiredCacheBus of
                Nothing                 -> DragonWaitCacheWrite -- Bus can't be acquired, keep trying to acquire on WaitCacheWrite state
                Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of
                    True    -> DragonWaitBusTr -- Bus is busy (line is shared, needs to update other copies), wait for it in WaitBusTr state
                    False   -> DragonDone -- Bus is not busy (line is not shared), go to Done state as no more operations need to be done
            Just SC -> case maybeAcquiredCacheBus of
                Nothing                 -> DragonWaitCacheWrite -- Bus can't be acquired, keep trying to acquire on WaitCacheWrite state
                Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of
                    True    -> DragonWaitBusTr -- Bus is busy (line is shared, needs to update other copies), wait for it in WaitBusTr state
                    False   -> DragonDone -- Bus is not busy (line is not shared), go to Done state as no more operations need to be done
            _       -> error "Cache hit on I state in Dragon Protocol"
        Just False  -> case maybeAcquiredCacheBus of -- Cache miss, next state depends on whether bus was acquired and bus busy state
            Nothing                 -> DragonWaitCacheWrite -- Bus can't be acquired, keep trying to acquire on WaitCacheWrite state
            Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of
                True    -> DragonWaitBusTr -- Bus is busy (line is shared, needs to wait for a copy from others), wait for it in WaitBusTr state
                False   -> DragonWaitMemoryRead -- Bus is not busy (line is not shared), issue a memory read and go to WaitMemoryRead state
        Nothing     -> DragonWaitCacheWrite -- Cache is still busy, wait for it in WaitCacheWrite state

    newCache = case isCacheHit of
        Just True   -> case blockState of -- Cache hit, commit write might need to be done depending on the block state
            Just M  -> Cache.commitWrite memoryAddress cache -- Cache hit on M state, commit write
            Just E  -> Cache.commitWrite memoryAddress cache -- Cache hit on E state, commit write
            Just SM -> case maybeAcquiredCacheBus of -- Cache hit on SM state, commit write might need to be done depending on whether the bus is acquired
                Nothing                 -> cache -- Bus can't be acquired, do not commit write on this cycle
                Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, commit write and change to SM state if shared
                    True    -> Cache.busSetBlockState SM memoryAddress writeCommittedCache -- Bus is busy (updating to shared copies), change to SM state
                    False   -> writeCommittedCache -- Bus is not busy (no shared copies), keep in M state
                    where
                        writeCommittedCache = Cache.commitWrite memoryAddress cache
            Just SC -> case maybeAcquiredCacheBus of -- Cache hit on SC state, commit write might need to be done depending on whether the bus is acquired
                Nothing                 -> cache -- Bus can't be acquired, do not commit write on this cycle
                Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, commit write and change to SM state if shared
                    True    -> Cache.busSetBlockState SM memoryAddress writeCommittedCache -- Bus is busy (updating to shared copies), change to SM state
                    False   -> writeCommittedCache -- Bus is not busy (no shared copies), keep in M state
                    where
                        writeCommittedCache = Cache.commitWrite memoryAddress cache
            _       -> error "Cache hit on I state in Dragon Protocol"
        Just False  -> cache -- Cache miss, no changes to cache on this cycle
        Nothing     -> cache -- Cache is still busy, no changes to cache on this cycle

    newMemory = case isCacheHit of
        Just True   -> memory -- Cache hit, no memory operations required
        Just False  -> case maybeAcquiredCacheBus of -- Cache miss, memory read might need to be issued depending on whether bus was acquired and bus busy state
            Nothing                 -> memory -- Bus can't be acquired, no need to issue memory read on this cycle
            Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, might need to issue memory read depending on bus busy state
                True    -> memory -- Bus is busy (line is shared, waiting for copy from others), no memory read needs to be issued
                False   -> Memory.issueRead memory -- Bus is not busy (line is not shared), need to issue a memory read to get data
        Nothing     -> memory -- Cache is still busy, no need to issue memory read on this cycle

    newCacheBus = case isCacheHit of
        Just True   -> case blockState of
            Just M  -> cacheBus -- Cache hit on M state, no bus transaction generated, return the same bus
            Just E  -> cacheBus -- Cache hit on E state, no bus transaction generated, return the same bus
            Just SM -> case maybeAcquiredCacheBus of
                Nothing                 -> cacheBus -- Bus can't be acquired, no changes to cache bus
                Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, need to return acquired/released bus depending on bus busy state
                    True    -> acquiredCacheBus -- Bus is busy (need to update other copies), return the acquired cache bus
                    False   -> Bus.release acquiredCacheBus -- Bus is not busy (no need to update other copies), Done state reached, release bus
            Just SC -> case maybeAcquiredCacheBus of
                Nothing                 -> cacheBus
                Just acquiredCacheBus   -> case Bus.isBusy acquiredCacheBus of -- Bus acquired, need to return acquired/released bus depending on bus busy state
                    True    -> acquiredCacheBus -- Bus is busy (need to update other copies), return the acquired cache bus
                    False   -> Bus.release acquiredCacheBus -- Bus is not busy (no need to update other copies), Done state reached, release bus
            _       -> error "Cache hit on I state in Dragon Protocol"
        Just False  -> case maybeAcquiredCacheBus of
            Nothing                 -> cacheBus -- Bus can't be acquired, no changes to cache bus
            Just acquiredCacheBus   -> acquiredCacheBus -- Bus acquired, return the acquired bus
        Nothing     -> cacheBus -- Cache is still busy, no changes to cache bus on this cycle

store (Just DragonWaitCacheRewrite) memoryAddress cache memory cacheBus = (newDragonState, newCache, memory, newCacheBus) where
    isCacheHit = Cache.isCacheHit cache
    blockState = Cache.busGetBlockState memoryAddress cache

    issuedCacheBus = case isCacheHit of
        Just True   -> case blockState of
            Just M  -> error "Cache rewrite on M state in Dragon Protocol"
            Just E  -> cacheBus -- Cache hit on E state, no need to issue bus transaction
            Just SM -> error "Cache rewrite on SM state in Dragon Protocol"
            Just SC -> Bus.issue (DragonBusUpd memoryAddress) cacheBus -- Cache hit on SC state, issue BusUpd
            _       -> error "Cache hit on I state in Dragon Protocol"
        Just False  -> error "Cache miss on cache rewrite in Dragon Protocol"
        Nothing     -> cacheBus -- Cache is still busy, no need to issue bus transaction on this cycle

    newDragonState = case isCacheHit of
        Just True   -> case blockState of -- Cache hit, next state depends on the block state and whether bus was acquired
            Just M  -> error "Cache rewrite on M state in Dragon Protocol"
            Just E  -> DragonDone -- Cache hit on E state, go to Done state as no more operations need to be done
            Just SM -> error "Cache rewrite on SM state in Dragon Protocol"
            Just SC -> case Bus.isBusy issuedCacheBus of
                True    -> DragonWaitBusTr -- Bus is busy (line is shared, needs to update other copies), wait for it in WaitBusTr state
                False   -> DragonDone -- Bus is not busy (line is not shared), go to Done state as no more operations need to be done
            _       -> error "Cache hit on I state in Dragon Protocol"
        Just False  -> error "Cache miss on cache rewrite in Dragon Protocol"
        Nothing     -> DragonWaitCacheRewrite -- Cache is still busy, wait for it in WaitCacheRewrite state

    newCache = case isCacheHit of
        Just True   -> case blockState of -- Cache hit, commit write might need to be done depending on the block state
            Just M  -> error "Cache rewrite on M state in Dragon Protocol"
            Just E  -> Cache.commitWrite memoryAddress cache -- Cache hit on E state, commit write
            Just SM -> error "Cache rewrite on SM state in Dragon Protocol"
            Just SC -> Cache.commitWrite memoryAddress cache -- Cache hit on SC state, needs to commit write as bus can always be issued (unlike acquire)
            _       -> error "Cache hit on I state in Dragon Protocol"
        Just False  -> error "Cache miss on cache rewrite in Dragon Protocol"
        Nothing     -> cache -- Cache is still busy, no changes to cache on this cycle

    newCacheBus = case isCacheHit of
        Just True   -> case blockState of
            Just M  -> error "Cache rewrite on M state in Dragon Protocol"
            Just E  -> Bus.release cacheBus -- Cache hit on E state, Done state reached, release the bus
            Just SM -> error "Cache rewrite on SM state in Dragon Protocol"
            Just SC -> case Bus.isBusy issuedCacheBus of -- Bus acquired, need to return issued/released bus depending on bus busy state
                True    -> issuedCacheBus -- Bus is busy (need to update other copies), return the issued cache bus
                False   -> Bus.release issuedCacheBus -- Bus is not busy (no need to update other copies), Done state reached, release bus
            _       -> error "Cache hit on I state in Dragon Protocol"
        Just False  -> error "Cache miss on cache rewrite"
        Nothing     -> cacheBus -- Cache is still busy, no changes to cache bus on this cycle

store (Just DragonWaitBusTr) memoryAddress cache memory cacheBus = (newDragonState, newCache, newMemory, newCacheBus) where
    isBusBusy = Bus.isBusy cacheBus
    blockState = Cache.busGetBlockState memoryAddress cache

    (maybeEvictedBlockState, cacheAfterAllocate) = case isBusBusy of
        True    -> (Nothing, cache) -- Bus is still busy, no need to allocate on this cycle
        False   -> case blockState of -- Bus is no longer busy, might need to allocate cache if it is needed
            Just M  -> error "BusUpd/BusRd on M state in Dragon Protocol"
            Just E  -> error "BusUpd/BusRd on E state in Dragon Protocol"
            Just SM -> (Nothing, cache) -- Address is already cached, no need to allocate
            Just SC -> (Nothing, cache) -- Address is already cached, no need to allocate
            _       -> Cache.busAllocate SC memoryAddress cache -- Address is not cached yet, allocate to cache in SC state (comes from Bus -> shared)

    newDragonState = case isBusBusy of
        True    -> DragonWaitBusTr -- Bus is still busy, wait for it on WaitBusTr state
        False   -> case blockState of -- Bus is no longer busy, next state depends on whether the address is cached
            Just M  -> error "BusUpd/BusRd on M state in Dragon Protocol"
            Just E  -> error "BusUpd/BusRd on E state in Dragon Protocol"
            Just SM -> DragonDone -- Address is cached, go to Done state as BusUpd operation is finished
            Just SC -> DragonDone -- Address is cached, go to Done state as BusUpd operation is finished
            _       -> case maybeEvictedBlockState of -- Address is not cached, the next state depends on whether an M block was evicted during allocation
                Just M  -> DragonWaitMemoryWrite -- An M block was evicted during allocation, issue a memory write and go to WaitMemoryWrite state
                _       -> DragonWaitCacheRewrite -- No M block was evicted during allocation, issue a cache rewrite and go to WaitCacheRewrite state

    newCache = case isBusBusy of
        True    -> cache -- Bus is still busy, no changes to cache
        False   -> case blockState of -- Bus is no longer busy, cache write might need to be issued depending on whether an M block was evicted
            Just M  -> error "BusUpd/BusRd on M state in Dragon Protocol"
            Just E  -> error "BusUpd/BusRd on E state in Dragon Protocol"
            Just SM -> cache -- Allocation was not done, no changes to cache
            Just SC -> cache -- Allocation was not done, no changes to cache
            _       -> case maybeEvictedBlockState of -- Allocation was done, cache write might need to be issued depending on whether an M block was evicted
                Just M  -> cacheAfterAllocate -- An M block was evicted during allocation, a memory write still needs to be done, no cache write issued yet
                _       -> Cache.issueWrite memoryAddress cacheAfterAllocate -- No M block evicted during allocation, issue a cache write

    newMemory = case isBusBusy of
        True    -> memory
        False   -> case blockState of -- Bus is no longer busy, might need to issue memory write depending on whether an M block was evicted on allocation
            Just M  -> error "BusUpd/BusRd on M state in Dragon Protocol"
            Just E  -> error "BusUpd/BusRd on E state in Dragon Protocol"
            Just SM -> memory -- Address is already cached, no allocation done
            Just SC -> memory -- Address is already cached, no allocation done
            _       -> case maybeEvictedBlockState of -- Address is not cached, might need to issue memory write if an M block was evicted
                Just M  -> Memory.issueWrite memory -- An M block was evicted during allocation, issue a memory write
                _       -> memory -- No M block was evicted during allocation, no need to issue a memory write

    newCacheBus = case isBusBusy of
        True    -> cacheBus -- Bus is still busy, no changes to bus
        False   -> case blockState of
            Just M  -> error "BusUpd/BusRd on M state in Dragon Protocol"
            Just E  -> error "BusUpd/BusRd on E state in Dragon Protocol"
            Just SM -> Bus.release cacheBus -- Address is cached, BusUpd operation is finished, Done state reached, release bus
            Just SC -> Bus.release cacheBus -- Address is cached, BusUpd operation is finished, Done state reached, release bus
            _       -> cacheBus -- Do not release bus yet as Bus.issue is needed in cache rewrite stage

store (Just DragonWaitMemoryRead) memoryAddress cache memory cacheBus = (newDragonState, newCache, newMemory, cacheBus) where
    isMemoryBusy = Memory.isBusy memory

    (maybeEvictedBlockState, cacheAfterAllocate) = case isMemoryBusy of
        True    -> (Nothing, cache) -- Memory is still busy, no allocation on this cycle
        False   -> Cache.busAllocate E memoryAddress cache -- Memory is no longer busy, allocate memory in cache on E state (comes from Memory -> not shared)

    newDragonState = case isMemoryBusy of
        True    -> DragonWaitMemoryRead -- Memory is still busy, wait for it in WaitMemoryRead state
        False   -> case maybeEvictedBlockState of -- Memory is no longer busy, next state depends on whether an M block was evicted on allocation
            Just M  -> DragonWaitMemoryWrite -- An M block was evicted on allocation, issue a memory write and go to WaitMemoryWrite
            _       -> DragonWaitCacheRewrite -- No M block was evicted on allocation, issue a cache write and go to WaitCacheRewrite

    newCache = case isMemoryBusy of
        True    -> cache -- Memory is still busy, no changes to cache
        False   -> case maybeEvictedBlockState of -- Memory is no longer busy, might need to issue cache write depending on whether M block was evicted
            Just M  -> cacheAfterAllocate -- An M block was evicted on allocation, do not issue cache write yet as a memory write still needs to be done
            _       -> Cache.issueWrite memoryAddress cacheAfterAllocate -- No M block was evicted, issue a cache write on the newly allocated block

    newMemory = case isMemoryBusy of
        True    -> memory -- Memory is still busy, no changes to memory on this cycle
        False   -> case maybeEvictedBlockState of -- Memory is no longer busy, might need to issue memory write depending on whether M block was evicted
            Just M  -> Memory.issueWrite memory -- An M block was evicted, issue a memory write
            _       -> memory -- No M block was evicted, no need to issue a memory write

store (Just DragonWaitMemoryWrite) memoryAddress cache memory cacheBus = (newDragonState, newCache, memory, cacheBus) where
    isMemoryBusy = Memory.isBusy memory

    newDragonState = case isMemoryBusy of
        True    -> DragonWaitMemoryWrite -- Memory is still busy, wait for it in WaitMemoryWrite state
        False   -> DragonWaitCacheRewrite -- Memory is no longer busy, immediately issue a cache write and go to WaitCacheRewrite

    newCache = case isMemoryBusy of
        True    -> cache -- Memory is still busy, no changes to cache
        False   -> Cache.issueWrite memoryAddress cache -- Memory is no longer busy, immediately issue a cache write

store (Just DragonWaitCacheRead) _ _ _ _ = error "WaitCacheRead state on DragonProtocol.store"
store (Just DragonDone) _ _ _ _ = error "Done state on DragonProtocol.store"
