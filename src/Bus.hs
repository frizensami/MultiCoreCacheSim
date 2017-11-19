{-# LANGUAGE ExistentialQuantification #-}

-- |This module defines methods for the shared bus between processors
module Bus where

import qualified Data.Sequence as S
import Data.Sequence ((|>), ViewR((:>)))
import Definitions
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

-- Creating CacheEventBuses
createNewCacheEventBus :: [Cache] -> Memory  -> CacheEventBus
createNewCacheEventBus caches memory = CacheEventBus createBus caches memory True False

-- Method to re-create the cache event bus on every new cycle
recreateCacheEventBus :: CacheEventBus -> [Cache] -> CacheEventBus 
recreateCacheEventBus (CacheEventBus bus _ memory isfree isbusy) caches = 
    CacheEventBus bus caches memory isfree isbusy

-- Public method for others to enqueue onto the cache event bus
enqueueCacheEventBus :: CacheEventBus -> BusEvent -> CacheEventBus 
enqueueCacheEventBus (CacheEventBus bus caches memory isfree isbusy) event = 
    CacheEventBus (enqueueBus bus event) caches memory isfree isbusy

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


