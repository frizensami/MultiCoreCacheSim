-- |This module defines methods for the shared bus between processors
module Bus
    ( createBus
    , enqueueBus
    , dequeueBus
    , isBusEmpty
    , EventBus(..)
    , CacheEventBus
    , BusEvent
    , MESIEvent
    , DragonEvent
    ) where

import qualified Data.Sequence as S
import Data.Sequence ((|>), ViewR((:>)))
import Definitions

-- Basic bus data types
newtype EventBus a = EventBus { getSeq :: (S.Seq a) } deriving (Show, Eq)
type CacheEventBus = EventBus String

-- Bus EVENT data types
-- data BusEvent  = BusEventMESI MESIEvent | BusEventDragon DragonEvent

class BusEvent a where
    getBusEventAddress :: a -> Address

data MESIEventType = MESIBusRd | MESIBusRdX | MESIBusWB deriving (Show)
data MESIEvent = MESIEvent MESIEventType Address
instance BusEvent MESIEvent where
    getBusEventAddress (MESIEvent _ addr) = addr

data DragonEventType = DragonBusRd | DragonBusUpd | DragonBusPrRdMiss | DragonBusPrWrMiss | DragonBusWB deriving (Show)
data DragonEvent = DragonEvent DragonEventType Address
instance BusEvent DragonEvent where
    getBusEventAddress (DragonEvent _ addr) = addr

createBus :: EventBus a
createBus = EventBus S.empty

enqueueBus :: EventBus a -> a -> EventBus a
enqueueBus bus newEvent = EventBus $ (getSeq bus) |> newEvent


dequeueBus :: EventBus a -> (EventBus a, a)
dequeueBus bus = (EventBus remainingSeq, lastEvent) where
    (remainingSeq :> lastEvent) = S.viewr $ getSeq bus

isBusEmpty :: EventBus a -> Bool
isBusEmpty bus = S.null $ getSeq bus


