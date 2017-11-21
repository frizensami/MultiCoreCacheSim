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
                           , getCyclesToCompute     :: Int
                           }

instance Show Processor where
    show (Processor pid status _ stats cycles) = "Processor #" ++ show pid ++ ": Status - " ++ show status ++ {- ", Cache - " ++ (show cache) ++ -} ", Stats: " ++ show stats ++ ", ComputeCyclesLeft: " ++ show cycles

createProcessor ::  ProtocolInput ->  CacheSize -> Associativity -> BlockSize -> Int -> Processor
createProcessor protocolInput cacheSize associativity blockSize pid = 
    Processor pid Idle newCache (newProcessorStatistics pid) 0 where
        newCache = Cache.create cacheSize associativity blockSize

-- TO BE IMPLEMENTED - THIS IS FOR TESTING FLOW
runOneCycle :: Processor -> Maybe Trace -> CacheBus -> (Processor, HasConsumedTrace, CacheBus)
runOneCycle processor (Just trace) eventBus = 
        -- Run print
        T.trace (show processor ++ ": " ++ show trace) $
        -- First check if we are currently processing anything
        if getCyclesToCompute processor == 0
            -- If nothing is processing - we continue handling the new trace
            then handleTrace processor trace eventBus
            -- Otherwise, run one cycle from the remaining compute cycles
            else (newProcessor, hasCompleted, eventBus) where
                processResult = processOneComputeCycle processor
                newProcessor  = fst processResult
                hasCompleted  = snd processResult
-- If we receive no traces, consider self as "done"
runOneCycle processor Nothing eventBus = (processor, True, eventBus) 


-- | Sets up the current trace into the processor and then executes the cycle if it's an OtherInstruction
handleTrace :: Processor -> Trace -> CacheBus -> (Processor, HasConsumedTrace, CacheBus)
handleTrace (Processor pid status cache stats cycles) trace@(OtherInstruction computeCycles) eventBus = 
    runOneCycle newProcessor (Just trace) eventBus where
        newProcessor = Processor pid status cache stats computeCycles -- Set the compute cycles
-- Can't do anything about other instructions yet - do nothing except consume3
handleTrace processor _ eventBus = (processor, True, eventBus)

-- | Runs one compute cycle from the number of cycles left to do computation
processOneComputeCycle :: Processor -> (Processor, Bool)
processOneComputeCycle (Processor pid status cache stats cycles) = (newProcessor, isDone) where
    newProcessor = Processor pid status cache (addOneStatsComputeCycle stats) (max (cycles - 1) 0)
    isDone = getCyclesToCompute newProcessor == 0

addOneStatsComputeCycle :: ProcessorStatistics -> ProcessorStatistics
addOneStatsComputeCycle (ProcessorStatistics compute loadstore idle misscount pid) = ProcessorStatistics (compute + 1) loadstore idle misscount pid