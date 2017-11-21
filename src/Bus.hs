-- |This module defines methods for the shared bus between processors
module Bus (CacheBus, BusTr (..), create, updateCaches, acquire, release, isShared, elapse, isBusy) where

import Definitions
import Cache (Cache)
import qualified Cache
import CacheBlock (BlockState (..))
import Memory (Memory)
import qualified Memory

-- Problem-specific bus type
data CacheBus = CacheBus [Cache] Memory (Maybe BusTr) NumCycles

-- Simplified bus event type until I figure out how to make two separate types that 
-- 1. Can be treated as the same type in some circumstances but 
-- 2. Separate when putting into a list - aka a list can only have one of these typs (MESI or Dragon events)
data BusTr = MESIBusRd MemoryAddress | MESIBusRdX MemoryAddress | MESIBusUpg MemoryAddress | DragonBusRd MemoryAddress | DragonBusUpd MemoryAddress deriving (Show)

-- | Creating brand new cache event bus - likely to be only called once then updated with recreate method
create :: [Cache] -> Memory -> CacheBus
create caches memory = CacheBus caches memory Nothing 0

-- | Updates the caches in the specified event bus
updateCaches :: CacheBus -> [Cache] -> CacheBus 
updateCaches (CacheBus oldCaches oldMemory oldMaybeBusTr oldBusyCycles) newCaches =
    CacheBus newCaches oldMemory oldMaybeBusTr oldBusyCycles

-- | Method for processors to attempt unique access to the bus by supplying a bus event
--   Maybe return a new cache event bus to indicate we managed to lock the bus down for us
acquire :: BusTr -> CacheBus -> Maybe CacheBus
-- Case #1: MESI BusRd
acquire (MESIBusRd memoryAddress) (CacheBus oldCaches oldMemory Nothing _) = Just newCacheBus where
    newCacheBus = CacheBus newCaches newMemory newMaybeBusTr newBusyCycles where
        -- Change all M/E states to S state
        newCaches = recursivelyUpdateBlockState M S memoryAddress $ recursivelyUpdateBlockState E S memoryAddress oldCaches
        newMemory =
            if recursivelyCheckCachesForState M memoryAddress oldCaches
                then Memory.issueWrite oldMemory -- a write-back to memory is required, issue a memory write to make bus busy
                else oldMemory
        newMaybeBusTr = Just $ MESIBusRd memoryAddress
        newBusyCycles = 0
-- Case #2: MESI BusRdX
acquire (MESIBusRdX memoryAddress) (CacheBus oldCaches oldMemory Nothing _) = Just newCacheBus where
    newCacheBus = CacheBus newCaches newMemory newMaybeBusTr newBusyCycles where
        -- Change all M/E/S states to I state
        newCaches = recursivelyUpdateBlockState M I memoryAddress
            $ recursivelyUpdateBlockState E I memoryAddress
            $ recursivelyUpdateBlockState S I memoryAddress oldCaches
        newMemory =
            if recursivelyCheckCachesForState M memoryAddress oldCaches
                then Memory.issueWrite oldMemory -- a write-back to memory is required, issue a memory write to make bus busy
                else oldMemory
        newMaybeBusTr = Just $ MESIBusRdX memoryAddress
        newBusyCycles = 0
-- Case #3: MESI BusUpg
acquire (MESIBusUpg memoryAddress) (CacheBus oldCaches oldMemory Nothing _) = Just newCacheBus where
    newCacheBus = CacheBus newCaches oldMemory newMaybeBusTr newBusyCycles where
        -- Change all S states to I state
        newCaches = recursivelyUpdateBlockState S I memoryAddress oldCaches
        newMaybeBusTr = Just $ MESIBusUpg memoryAddress
        newBusyCycles = 0
-- Case #4: Dragon BusRd
acquire (DragonBusRd memoryAddress) (CacheBus oldCaches oldMemory Nothing _) = Nothing
-- Case #5: Dragon BusUpd
acquire (DragonBusUpd memoryAddress) (CacheBus oldCaches oldMemory Nothing _) = Nothing
-- Final case: bus is not free
acquire _ (CacheBus _ _ (Just _) _) = Nothing

recursivelyCheckCachesForState :: BlockState -> MemoryAddress -> [Cache] -> Bool
recursivelyCheckCachesForState blockState memoryAddress [] = False
recursivelyCheckCachesForState blockState memoryAddress (x:xs)
    | Cache.busGetBlockState memoryAddress x == Just blockState = True || recurrence
    | otherwise                                                 = False || recurrence
    where
        recurrence = recursivelyCheckCachesForState blockState memoryAddress xs

recursivelyUpdateBlockState :: BlockState -> BlockState -> MemoryAddress -> [Cache] -> [Cache]
recursivelyUpdateBlockState fromBlockState toBlockState memoryAddress [] = []
recursivelyUpdateBlockState fromBlockState toBlockState memoryAddress (x:xs)
    | Cache.busGetBlockState memoryAddress x == Just fromBlockState = (:[]) updatedCache ++ recurrence
    | otherwise                                                     = (:[]) x ++ recurrence
    where
        updatedCache = Cache.busSetBlockState toBlockState memoryAddress x
        recurrence = recursivelyUpdateBlockState fromBlockState toBlockState memoryAddress xs

-- |Releases the cache bus from being bound to a cache.
--  Returns the renewed cache bus.
release :: CacheBus -> CacheBus
release (CacheBus oldCaches oldMemory _ oldBusyCycles) = newCacheBus where
    newCacheBus = CacheBus oldCaches oldMemory Nothing oldBusyCycles

-- |Checks whether the specified memory address is shared by at least a cache.
--  Returns True if it is shared, False otherwise.
isShared :: MemoryAddress -> CacheBus -> Bool
isShared memoryAddress (CacheBus caches _ _ _) = isSharedS || isSharedSC where
    isSharedS = recursivelyCheckCachesForState S memoryAddress caches
    isSharedSC = recursivelyCheckCachesForState SC memoryAddress caches

-- |Elapses a single cycle from the bus memory and the bus itself.
--  Returns the renewed cache bus.
elapse :: CacheBus -> CacheBus
elapse (CacheBus oldCaches oldMemory oldMaybeBusTr oldBusyCycles) = newCacheBus where
    newCacheBus = CacheBus oldCaches newMemory oldMaybeBusTr newBusyCycles where
        newMemory = Memory.elapse oldMemory
        newBusyCycles = max (oldBusyCycles - 1) 0

-- |Checks if the bus is busy.
--  Returns True if either the bus memory or the bus itself is busy, False otherwise.
isBusy :: CacheBus -> IsBusy
isBusy (CacheBus _ memory _ busyCycles) = (Memory.isBusy memory) || (busyCycles /= 0)
