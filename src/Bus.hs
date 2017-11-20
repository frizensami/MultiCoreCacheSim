-- |This module defines methods for the shared bus between processors
module Bus where

import qualified Data.Sequence as S
import Data.Sequence ((|>), ViewR((:>)))
import Cache
import Memory

-- Basic bus data type
newtype EventBus a = EventBus { getSeq :: (S.Seq a) } deriving (Show, Eq)
-- Problem-specific bus type
data CacheEventBus = CacheEventBus { getEventBus :: EventBus BusEvent
                                   , getCaches   :: [Cache]
                                   , getMemory   :: Memory
                                   , isFree      :: Bool
                                   , isBusy      :: Bool
                                   }

-- Simplified bus event type until I figure out how to make two separate types that 
-- 1. Can be treated as the same type in some circumstances but 
-- 2. Separate when putting into a list - aka a list can only have one of these typs (MESI or Dragon events)
data BusEvent = MESIBusRd | MESIBusRdX  | DragonBusRd | DragonBusUpd deriving (Show)

-- | Creating brand new cache event bus - likely to be only called once then updated with recreate method
createNewCacheEventBus :: [Cache] -> Memory  -> CacheEventBus
createNewCacheEventBus caches memory = CacheEventBus createBus caches memory True False

-- | Method to re-create the cache event bus on every new cycle
recreateCacheEventBus :: CacheEventBus -> [Cache] -> CacheEventBus 
recreateCacheEventBus (CacheEventBus bus _ memory isfree isbusy) caches = 
    CacheEventBus bus caches memory isfree isbusy

-- | Public method for others to enqueue onto the cache event bus
enqueueEventBus :: CacheEventBus -> BusEvent -> CacheEventBus 
enqueueEventBus (CacheEventBus bus caches memory isfree isbusy) event = 
    CacheEventBus (enqueueBus bus event) caches memory isfree isbusy

-- | Method for processors to attempt unique access to the bus
--   Maybe return a new cache event bus to indicate we managed to lock the bus down for us
acquireEventBus :: CacheEventBus -> Maybe CacheEventBus
acquireEventBus (CacheEventBus bus caches memory isfree isbusy) = 
    if isfree && not isbusy 
        then Just (CacheEventBus bus caches memory False isbusy) -- Is no longer free!
        else Nothing

-- | Method for a processor to give up control over the bus. ASSUMES WELL BEHAVED CALLS.
--   No checking whether the processor that calls this method is authorized to do so!
--   Maybe return a new cache event bus to indicate we managed to release the bus.
releaseEventBus :: CacheEventBus -> Maybe CacheEventBus
releaseEventBus (CacheEventBus bus caches memory isfree isbusy) = 
    if not isfree -- do i need to check for busy?
        then Just (CacheEventBus bus caches memory True isbusy) -- Is no longer free!
        else Nothing

-- | MAIN METHOD FOR BUS TO DEQUEUE AN EVENT AND EXECUTE IT
-- TBI!
runEventBus :: CacheEventBus -> CacheEventBus 
runEventBus (CacheEventBus bus caches memory isfree isbusy) = error "TBI!"
                                                    

-- GENERAL BUS FUNCTIONS
createBus :: EventBus a
createBus = EventBus S.empty

enqueueBus :: EventBus a -> a -> EventBus a
enqueueBus bus newEvent = EventBus $ (getSeq bus) |> newEvent


dequeueBus :: EventBus a -> (EventBus a, a)
dequeueBus bus = (EventBus remainingSeq, lastEvent) where
    (remainingSeq :> lastEvent) = S.viewr $ getSeq bus

isBusEmpty :: EventBus a -> Bool
isBusEmpty bus = S.null $ getSeq bus


