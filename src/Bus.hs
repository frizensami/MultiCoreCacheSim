-- |This module defines methods for the shared bus between processors
module Bus (CacheBus, BusTr (..), getCacheBusCaches, getBusStats, create, updateCaches, acquire, issue, release, hasSharedCopies, elapse, isBusy) where

import Definitions
import Cache (Cache)
import qualified Cache
import qualified CacheParams
import CacheBlock (BlockState (..))
import Memory (Memory)
import qualified Memory
import Statistics

updateCycles :: Int
updateCycles = 2

-- Problem-specific bus type
data CacheBus = CacheBus [Cache] Memory (Maybe BusTr) NumCycles BusStatistics

getCacheBusCaches :: CacheBus -> [Cache]
getCacheBusCaches (CacheBus caches _ _ _ _) = caches

getBusStats :: CacheBus -> BusStatistics
getBusStats (CacheBus _ _ _ _ busStatistics) = busStatistics

-- Simplified bus event type until I figure out how to make two separate types that 
-- 1. Can be treated as the same type in some circumstances but 
-- 2. Separate when putting into a list - aka a list can only have one of these typs (MESI or Dragon events)
data BusTr = MESIBusRd MemoryAddress | MESIBusRdX MemoryAddress | MESIBusUpg MemoryAddress
    | DragonBusRd MemoryAddress | DragonBusUpd MemoryAddress
    | IllinoisBusRd MemoryAddress | IllinoisBusRdX MemoryAddress | IllinoisBusUpg MemoryAddress deriving (Show)

-- | Creating brand new cache event bus - likely to be only called once then updated with recreate method
create :: [Cache] -> Memory -> CacheBus
create caches memory = CacheBus caches memory Nothing 0 createBusStatistics

-- | Updates the caches in the specified event bus
updateCaches :: CacheBus -> [Cache] -> CacheBus 
updateCaches (CacheBus oldCaches oldMemory oldMaybeBusTr oldBusyCycles oldBusStatistics) newCaches =
    CacheBus newCaches oldMemory oldMaybeBusTr oldBusyCycles oldBusStatistics

-- | Method for processors to attempt unique access to the bus by supplying a bus event
--   Maybe return a new cache event bus to indicate we managed to lock the bus down for us
acquire :: BusTr -> CacheBus -> Maybe CacheBus
-- Case #1: MESI BusRd
acquire (MESIBusRd memoryAddress) (CacheBus oldCaches oldMemory Nothing _ oldBusStatistics) = Just newCacheBus where
    newCacheBus = CacheBus newCaches newMemory newMaybeBusTr newBusyCycles newBusStatistics where
        -- Change all M/E states to S state
        newCaches = recursivelyUpdateBlockState M S memoryAddress
            $ recursivelyUpdateBlockState E S memoryAddress oldCaches
        newMemory = case cachesHaveMState of
            True    -> Memory.issueWrite oldMemory -- a write-back to memory is required, issue a memory write to make bus busy
            False   -> oldMemory
        newMaybeBusTr = Just $ MESIBusRd memoryAddress
        newBusyCycles = 0
        newBusStatistics = case cachesHaveMState of
            True    -> addBusTrafficStats blockSize oldBusStatistics
            False   -> oldBusStatistics
            where
                blockSize = CacheParams.getBlockSize $ Cache.getCacheParams $ oldCaches!!0
        cachesHaveMState = recursivelyCheckCachesForState M memoryAddress oldCaches
-- Case #2: MESI BusRdX
acquire (MESIBusRdX memoryAddress) (CacheBus oldCaches oldMemory Nothing _ oldBusStatistics) = Just newCacheBus where
    newCacheBus = CacheBus newCaches newMemory newMaybeBusTr newBusyCycles newBusStatistics where
        -- Change all M/E/S states to I state
        newCaches = recursivelyUpdateBlockState M I memoryAddress
            $ recursivelyUpdateBlockState E I memoryAddress
            $ recursivelyUpdateBlockState S I memoryAddress oldCaches
        newMemory = case cachesHaveMState of
            True    -> Memory.issueWrite oldMemory -- a write-back to memory is required, issue a memory write to make bus busy
            False   -> oldMemory
        newMaybeBusTr = Just $ MESIBusRdX memoryAddress
        newBusyCycles = 0
        newBusStatistics = case cachesHaveMState of
            True    -> addBusTrafficStats blockSize $ incrementBusIvUpdStats oldBusStatistics
            False   -> incrementBusIvUpdStats oldBusStatistics
            where
                blockSize = CacheParams.getBlockSize $ Cache.getCacheParams $ oldCaches!!0
        cachesHaveMState = recursivelyCheckCachesForState M memoryAddress oldCaches
-- Case #3: MESI BusUpg
acquire (MESIBusUpg memoryAddress) (CacheBus oldCaches oldMemory Nothing _ oldBusStatistics) = Just newCacheBus where
    newCacheBus = CacheBus newCaches oldMemory newMaybeBusTr newBusyCycles newBusStatistics where
        -- Change all S states to I state
        newCaches = recursivelyUpdateBlockState S I memoryAddress oldCaches
        newMaybeBusTr = Just $ MESIBusUpg memoryAddress
        newBusyCycles = 0
        newBusStatistics = incrementBusIvUpdStats oldBusStatistics
-- Case #4: Dragon BusRd
acquire (DragonBusRd memoryAddress) (CacheBus oldCaches oldMemory Nothing oldBusyCycles oldBusStatistics) = Just newCacheBus where
    newCacheBus = CacheBus newCaches newMemory newMaybeBusTr newBusyCycles newBusStatistics where
        -- Change all M states to SM state, all E states to SC state
        newCaches = recursivelyUpdateBlockState M SM memoryAddress
            $ recursivelyUpdateBlockState E SC memoryAddress oldCaches
        newMemory = case cachesHaveModifiedCopies of
            True    -> Memory.issueWrite oldMemory -- an SM/M state exists in other cache, write back from that cache to memory
            False   -> oldMemory -- no SM/M state exists in other cache, no need to issue memory write
            where
                cachesHaveModifiedCopies = hasModifiedCopies memoryAddress $ CacheBus oldCaches oldMemory Nothing oldBusyCycles oldBusStatistics
        newMaybeBusTr = Just $ DragonBusRd memoryAddress
        newBusyCycles = case cachesHaveCopies of
            True    -> updateCycles * wordsPerBlock -- Cached in other caches, bus busy cycles set to updateCycles * N (forwarding entire block of N words)
            False   -> 0 -- Not cached in other caches, bus busy cycles is 0 (instant bus operation)
            where
                wordsPerBlock = blockSize `div` 4
        newBusStatistics = case cachesHaveCopies of
            True    -> (incrementBusIvUpdStats . addBusTrafficStats blockSize) oldBusStatistics
            False   -> oldBusStatistics
        cachesHaveCopies = hasCopies memoryAddress $ CacheBus oldCaches oldMemory Nothing oldBusyCycles oldBusStatistics
        blockSize = CacheParams.getBlockSize $ Cache.getCacheParams $ oldCaches!!0
-- Case #5: Dragon BusUpd
acquire (DragonBusUpd memoryAddress) (CacheBus oldCaches oldMemory Nothing oldBusyCycles oldBusStatistics) = Just newCacheBus where
    newCacheBus = CacheBus newCaches oldMemory newMaybeBusTr newBusyCycles newBusStatistics where
        -- Change all SM states to SC state
        newCaches = recursivelyUpdateBlockState SM SC memoryAddress oldCaches
        newMaybeBusTr = Just $ DragonBusUpd memoryAddress
        newBusyCycles = case cachesHaveSharedCopies of
            True    -> updateCycles -- the memory address exists in shared states in other caches, bus busy cycles set to updateCycles (forwarding a word)
            False   -> 0 -- the memory address is not cached in other caches, bus busy cycles is 0 (instant bus operation)
        newBusStatistics = case cachesHaveSharedCopies of
            True    -> addBusTrafficStats 4 $ incrementBusIvUpdStats oldBusStatistics -- exists in shared states in other caches, need to update
            False   -> oldBusStatistics -- doesnt exist in other caches, no need to update
        cachesHaveSharedCopies = hasSharedCopies memoryAddress $ CacheBus oldCaches oldMemory Nothing oldBusyCycles oldBusStatistics
acquire (IllinoisBusRd memoryAddress) (CacheBus oldCaches oldMemory Nothing oldBusyCycles oldBusStatistics) = Just newCacheBus where
    newCacheBus = CacheBus newCaches newMemory newMaybeBusTr newBusyCycles newBusStatistics where
        -- Change all M/E states to S state
        newCaches = recursivelyUpdateBlockState M S memoryAddress
            $ recursivelyUpdateBlockState E S memoryAddress oldCaches
        newMemory = case cachesHaveMState of
            True    -> Memory.issueWrite oldMemory -- a write-back to memory is required, issue a memory write to make bus busy
            False   -> oldMemory
        newMaybeBusTr = Just $ IllinoisBusRd memoryAddress
        newBusyCycles = case cachesHaveCopies of
            True    -> updateCycles * wordsPerBlock
            False   -> 0
            where
                wordsPerBlock = blockSize `div` 4
        newBusStatistics = case cachesHaveCopies of
            True    -> (incrementBusIvUpdStats . addBusTrafficStats blockSize) oldBusStatistics
            False   -> oldBusStatistics
        cachesHaveMState = recursivelyCheckCachesForState M memoryAddress oldCaches
        cachesHaveCopies = hasCopies memoryAddress $ CacheBus oldCaches oldMemory Nothing oldBusyCycles oldBusStatistics
        blockSize = CacheParams.getBlockSize $ Cache.getCacheParams $ oldCaches!!0
acquire (IllinoisBusRdX memoryAddress) (CacheBus oldCaches oldMemory Nothing oldBusyCycles oldBusStatistics) = Just newCacheBus where
    newCacheBus = CacheBus newCaches newMemory newMaybeBusTr newBusyCycles newBusStatistics where
        -- Change all M/E/S states to I state
        newCaches = recursivelyUpdateBlockState M I memoryAddress
            $ recursivelyUpdateBlockState E I memoryAddress
            $ recursivelyUpdateBlockState S I memoryAddress oldCaches
        newMemory = case cachesHaveMState of
            True    -> Memory.issueWrite oldMemory -- a write-back to memory is required, issue a memory write to make bus busy
            False   -> oldMemory
        newMaybeBusTr = Just $ IllinoisBusRdX memoryAddress
        newBusyCycles = case cachesHaveCopies of
            True    -> updateCycles * wordsPerBlock
            False   -> 0
            where
                wordsPerBlock = blockSize `div` 4
        newBusStatistics = case cachesHaveCopies of
            True    -> addBusTrafficStats blockSize $ incrementBusIvUpdStats oldBusStatistics
            False   -> incrementBusIvUpdStats oldBusStatistics
        cachesHaveMState = recursivelyCheckCachesForState M memoryAddress oldCaches
        cachesHaveCopies = hasCopies memoryAddress $ CacheBus oldCaches oldMemory Nothing oldBusyCycles oldBusStatistics
        blockSize = CacheParams.getBlockSize $ Cache.getCacheParams $ oldCaches!!0
acquire (IllinoisBusUpg memoryAddress) (CacheBus oldCaches oldMemory Nothing _ oldBusStatistics) = Just newCacheBus where
    newCacheBus = CacheBus newCaches oldMemory newMaybeBusTr newBusyCycles newBusStatistics where
        -- Change all S states to I state
        newCaches = recursivelyUpdateBlockState S I memoryAddress oldCaches
        newMaybeBusTr = Just $ IllinoisBusUpg memoryAddress
        newBusyCycles = 0
        newBusStatistics = incrementBusIvUpdStats oldBusStatistics
-- Final case: bus is not free
acquire _ (CacheBus _ _ (Just _) _ _) = Nothing

-- | Method for processors to issue another bus event on a currently acquired bus.
--   Returns a new cache event bus.
issue :: BusTr -> CacheBus -> CacheBus
issue (DragonBusUpd memoryAddress) (CacheBus oldCaches oldMemory (Just _) oldBusyCycles oldBusStatistics)
    = (CacheBus oldCaches oldMemory Nothing oldBusyCycles oldBusStatistics)
issue _ (CacheBus _ _ _ _ _) = error "Issue of non-supported bus transaction or on an unacquired bus"

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
release (CacheBus oldCaches oldMemory _ oldBusyCycles oldBusStatistics) = newCacheBus where
    newCacheBus = CacheBus oldCaches oldMemory Nothing oldBusyCycles oldBusStatistics

hasCopies :: MemoryAddress -> CacheBus -> Bool
hasCopies memoryAddress (CacheBus caches _ _ _ _) = hasS || hasSC || hasSM || hasE || hasM where
    hasS = recursivelyCheckCachesForState S memoryAddress caches
    hasSC = recursivelyCheckCachesForState SC memoryAddress caches
    hasSM = recursivelyCheckCachesForState SM memoryAddress caches
    hasE = recursivelyCheckCachesForState E memoryAddress caches
    hasM = recursivelyCheckCachesForState M memoryAddress caches

-- |Checks whether the specified memory address is shared by at least a cache.
--  Returns True if it is shared, False otherwise.
hasSharedCopies :: MemoryAddress -> CacheBus -> Bool
hasSharedCopies memoryAddress (CacheBus caches _ _ _ _) = hasS || hasSC || hasSM where
    hasS = recursivelyCheckCachesForState S memoryAddress caches
    hasSC = recursivelyCheckCachesForState SC memoryAddress caches
    hasSM = recursivelyCheckCachesForState SM memoryAddress caches

-- |Checks whether the specified memory address is cached in modified by at least a cache.
--  Returns True if it is modified somewhere, False otherwise.
hasModifiedCopies :: MemoryAddress -> CacheBus -> Bool
hasModifiedCopies memoryAddress (CacheBus caches _ _ _ _) = hasM || hasSM where
    hasM = recursivelyCheckCachesForState M memoryAddress caches
    hasSM = recursivelyCheckCachesForState SM memoryAddress caches

-- |Elapses a single cycle from the bus memory and the bus itself.
--  Returns the renewed cache bus.
elapse :: CacheBus -> CacheBus
elapse (CacheBus oldCaches oldMemory oldMaybeBusTr oldBusyCycles oldBusStatistics) = newCacheBus where
    newCacheBus = CacheBus oldCaches newMemory oldMaybeBusTr newBusyCycles oldBusStatistics where
        newMemory = Memory.elapse oldMemory
        newBusyCycles = max (oldBusyCycles - 1) 0

-- |Checks if the bus is busy.
--  Returns True if either the bus memory or the bus itself is busy, False otherwise.
isBusy :: CacheBus -> IsBusy
isBusy (CacheBus _ memory _ busyCycles _) = (Memory.isBusy memory) || (busyCycles /= 0)
