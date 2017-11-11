-- |This module defines methods for the shared bus between processors
module Bus
    ( createBus
    , enqueueBus
    , dequeueBus
    , isBusEmpty
    , EventBus(..)
    ) where

import qualified Data.Sequence as S
import Data.Sequence ((|>), ViewR((:>)))

newtype EventBus a = EventBus { getSeq :: (S.Seq a) } deriving (Show, Eq)

createBus :: EventBus a
createBus = EventBus S.empty

enqueueBus :: EventBus a -> a -> EventBus a
enqueueBus bus newEvent = EventBus $ (getSeq bus) |> newEvent


dequeueBus :: EventBus a -> (EventBus a, a)
dequeueBus bus = (EventBus remainingSeq, lastEvent) where
    (remainingSeq :> lastEvent) = S.viewr $ getSeq bus

isBusEmpty :: EventBus a -> Bool
isBusEmpty bus = S.null $ getSeq bus


