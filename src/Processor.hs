-- |This module defines methods for interacting with and creating processors
module Processor 
    ( createProcessor
    , runOneCycle
    , ProcessorStatus(..)
    , Processor(..)
    ) where

import Definitions
import Trace
import Bus
import Statistics
import qualified Debug.Trace as T
import Cache (Cache)
import qualified Cache as Cache

type HasConsumedTrace = Bool

data ProcessorStatus = Idle | Blocked | Running deriving (Show)
data Processor = Processor { getProcessorID         :: Int
                           , getProcessorStatus     :: ProcessorStatus
                           , getCache               :: Cache
                           , getProcessorStatistics :: ProcessorStatistics
                           }

instance Show Processor where
    show (Processor pid status cache stats) = "Processor #" ++ (show pid) ++ ": Status - " ++ (show status) ++ ", Cache - " ++ (show cache) ++ ", Stats: " ++ (show stats)

createProcessor ::  ProtocolInput ->  CacheSize -> Associativity -> BlockSize -> Int -> Processor
createProcessor protocolInput cacheSize associativity blockSize pid = 
    Processor pid Idle newCache (newProcessorStatistics pid) where
        newCache = Cache.create cacheSize associativity blockSize

-- TO BE IMPLEMENTED - THIS IS FOR TESTING FLOW
runOneCycle :: Processor -> Maybe Trace -> CacheEventBus -> (Processor, HasConsumedTrace, CacheEventBus)
runOneCycle processor (Just trace) eventBus = T.trace ((show processor) ++ ": " ++ (show trace)) $ (processor, True, eventBus) 
runOneCycle processor Nothing eventBus      = (processor, True, eventBus) 