-- |This module defines methods for interacting with and creating processors
module Processor 
    ( createProcessor
    , runOneCycle
    , Processor(..)
    ) where

import Definitions
import Trace
import Bus
import Statistics
import qualified Debug.Trace as T
import Cache (Cache)
import qualified Cache as Cache
import Protocols
import qualified MESIProtocol
import Memory 

type HasConsumedTrace = Bool

data Processor = Processor { getProcessorID         :: Int
                           , getProtocol            :: Protocol
                           , getCache               :: Cache
                           , getProcessorStatistics :: ProcessorStatistics
                           , getCyclesToCompute     :: Int
                           , protocolState          :: Maybe ProtocolState
                           , memory                 :: Memory
                           }

instance Show Processor where
    show (Processor pid protocol _ stats cycles protocolstate _) = "Processor #" ++ show pid ++ ": Status - " ++ show protocol ++ {- ", Cache - " ++ (show cache) ++ -} ", Stats: " ++ show stats ++ ", ComputeCyclesLeft: " ++ show cycles ++ ", Protocol State: " ++ show protocolstate

createProcessor ::  ProtocolInput ->  CacheSize -> Associativity -> BlockSize -> Int -> Processor
createProcessor protocolInput cacheSize associativity blockSize pid = 
    Processor pid protocol newCache (newProcessorStatistics pid) 0 Nothing Memory.create where
        newCache = Cache.create cacheSize associativity blockSize
        protocol = case protocolInput of
            "MESI"   -> MESI
            "Dragon" -> Dragon
            _        -> error "PANIC! Unrecognized protocol!"

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
handleTrace (Processor pid protocol cache stats cycles pstate mem) trace@(OtherInstruction computeCycles) eventBus = 
    runOneCycle newProcessor (Just trace) eventBus where
        newProcessor = Processor pid protocol cache stats computeCycles pstate mem-- Set the compute cycles
{-
handleTrace (Processor pid MESI cache stats cycles pstate mem) trace@(LoadInstruction address) eventBus = (newProcessor, hasConsumedTrace, newBus) where
    (newPState, newCache, newMemory, newBus) = case pstate of
        Nothing -> MESIProtocol.load Nothing address cache mem eventBus
-}
-- Can't do anything about other instructions yet - do nothing except consume3
handleTrace processor _ eventBus = (processor, True, eventBus)

-- | Runs one compute cycle from the number of cycles left to do computation
processOneComputeCycle :: Processor -> (Processor, Bool)
processOneComputeCycle (Processor pid protocol cache stats cycles pstate mem) = (newProcessor, isDone) where
    newProcessor = Processor pid protocol cache (addOneStatsComputeCycle stats) (max (cycles - 1) 0) pstate mem
    isDone = getCyclesToCompute newProcessor == 0

addOneStatsComputeCycle :: ProcessorStatistics -> ProcessorStatistics
addOneStatsComputeCycle (ProcessorStatistics compute loadstore idle misscount pid) = ProcessorStatistics (compute + 1) loadstore idle misscount pid