-- |This module defines methods for interacting with and creating processors
module Processor 
    ( createProcessor
    , runOneCycle
    , Processor(..)
    , updateProcessorCache
    ) where

import Definitions
import Trace
import Bus
import Statistics
import qualified Debug.NoTrace as T
import Cache (Cache)
import qualified Cache as Cache
import Protocols
import qualified MESIProtocol
import Memory 
import Protocol
import qualified DragonProtocol
import Data.Maybe (isNothing)
import qualified IllinoisProtocol

type HasConsumedTrace = Bool

data Processor = Processor { getProcessorID         :: Int
                           , getProtocol            :: Protocol
                           , getCache               :: Cache
                           , getProcessorStatistics :: ProcessorStatistics
                           , getCyclesToCompute     :: Int
                           , protocolState          :: Maybe ProtocolStates
                           , memory                 :: Memory
                           }

instance Show Processor where
    show (Processor pid protocol _ stats cycles protocolstate _) = "Processor #" ++ show pid ++ {- ": Status - " ++ show protocol ++ {- ", Cache - " ++ (show cache) ++ -} ", Stats: " ++ show stats ++ ", ComputeCyclesLeft: " ++ show cycles ++ -} ",  Protocol State: " ++ show protocolstate

createProcessor ::  ProtocolInput ->  CacheSize -> Associativity -> BlockSize -> Int -> Processor
createProcessor protocolInput cacheSize associativity blockSize pid = 
    Processor pid protocol newCache (newProcessorStatistics pid) 0 Nothing Memory.create where
        newCache = Cache.create cacheSize associativity blockSize
        protocol = case protocolInput of
            "MESI"     -> MESI
            "Dragon"   -> Dragon
            "Illinois" -> Illinois
            _        -> error "PANIC! Unrecognized protocol!"

-- | Used to update processors with a new cache from the protocol on every cycle
updateProcessorCache :: Processor -> Cache -> Processor
updateProcessorCache (Processor pid protocol _ stats cycles pstate mem) newCache =
    Processor pid protocol newCache stats cycles pstate mem


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
handleTrace (Processor pid protocol cache stats _ pstate mem) trace@(OtherInstruction computeCycles) eventBus = 
    runOneCycle newProcessor (Just trace) eventBus where
        newProcessor = Processor pid protocol cache stats computeCycles pstate mem-- Set the compute cycles

-- | If it's any other instruction, choose which protocol should execute it and run the load/store fx
handleTrace (Processor pid MESI cache stats cycles pstate mem) trace eventBus = (newProcessor, hasConsumedTrace, newBus) where
    -- Run the protocol function and update internal state
    -- Select which protocol function to execute
    !(protocolFunction, address) = case trace of
        LoadInstruction  addr     -> (MESIProtocol.load,  addr)
        StoreInstruction addr     -> (MESIProtocol.store, addr)
        t                         -> error "Not expecing OtherInstruction inside handleTrace!"
    -- Run the protocol function and update internal state
    !(newPState, newCache, newMemory, newBus) = case pstate of
        Nothing                       -> ($!) protocolFunction Nothing address cache mem eventBus pid
        Just (MESIProtocol mesiState) -> ($!) protocolFunction  (Just mesiState) address cache mem eventBus pid
        x                             -> error $ "handleTrace does not support state " ++ show x
    -- Design decision to NOT update the cache here since we'd have to check for equality through entire array
    -- Run a tick for the cache and memory
    !elapsedCache  = Cache.elapse newCache 
    !elapsedMemory = Memory.elapse newMemory
    -- Update internal state and return values
    !hasConsumedTrace = isDone (newPState :: MESIProtocol.MESIState)

    -- Update statistics: 
    -- IF we receive a LOAD or STORE and we are in a NOTHING stage we must have 
    -- received a new load/store instruction. Update our statistics
    !statsWithLoadStore = if isNothing pstate then addOneStatsLoadStoreCycle stats else stats
    -- If we in in this state, we MUST be idling waiting for cache
    !statsWithIdleCycle = addOneStatsIdleCycle statsWithLoadStore
    -- If our state is nothing, check with the cache if we have missed 
    !statsWithMissRate  = if isNothing pstate then 
            case Cache.busGetBlockState address cache of 
                Nothing -> addOneStatsMissCount statsWithIdleCycle
                _ -> statsWithIdleCycle
            else statsWithIdleCycle
    !finalpstate = if hasConsumedTrace then Nothing else Just $ MESIProtocol newPState 
    !newProcessor  = Processor pid MESI elapsedCache statsWithMissRate cycles finalpstate elapsedMemory

-- | If it's any other instruction, choose which protocol should execute it and run the load/store fx
handleTrace (Processor pid Dragon cache stats cycles pstate mem) trace eventBus = (newProcessor, hasConsumedTrace, newBus) where
    -- Run the protocol function and update internal state
    -- Select which protocol function to execute
    (protocolFunction, address) = case trace of
        LoadInstruction  addr     -> (DragonProtocol.load,  addr)
        StoreInstruction addr     -> (DragonProtocol.store, addr)
        t                         -> error "Not expecing OtherInstruction inside handleTrace!"
    -- Run the protocol function and update internal state
    (newPState, newCache, newMemory, newBus) = case pstate of
        Nothing                           -> protocolFunction Nothing address cache mem eventBus pid
        Just (DragonProtocol dragonState) -> protocolFunction (Just dragonState) address cache mem eventBus pid
        x                                 -> error $ "handleTrace does not support state " ++ show x
    -- Design decision to NOT update the cache here since we'd have to check for equality through entire array
    -- Run a tick for the cache and memory
    elapsedCache  = Cache.elapse newCache 
    elapsedMemory = Memory.elapse newMemory
    -- Update internal state and return values
    hasConsumedTrace = isDone newPState

    -- Update statistics: 
    -- IF we receive a LOAD or STORE and we are in a NOTHING stage we must have 
    -- received a new load/store instruction. Update our statistics
    statsWithLoadStore = if isNothing pstate then addOneStatsLoadStoreCycle stats else stats
    -- If we in in this state, we MUST be idling waiting for cache
    statsWithIdleCycle = addOneStatsIdleCycle statsWithLoadStore
    -- If our state is nothing, check with the cache if we have missed 
    statsWithMissRate  = if isNothing pstate then 
            case Cache.busGetBlockState address cache of 
                Nothing -> addOneStatsMissCount statsWithIdleCycle
                _ -> statsWithIdleCycle
            else statsWithIdleCycle
    finalpstate = if hasConsumedTrace then Nothing else Just $ DragonProtocol newPState 
    newProcessor  = Processor pid Dragon elapsedCache statsWithMissRate cycles finalpstate elapsedMemory

-- | If it's any other instruction, choose which protocol should execute it and run the load/store fx
handleTrace (Processor pid Illinois cache stats cycles pstate mem) trace eventBus = (newProcessor, hasConsumedTrace, newBus) where
    -- Run the protocol function and update internal state
    -- Select which protocol function to execute
    (protocolFunction, address) = case trace of
        LoadInstruction  addr     -> (IllinoisProtocol.load,  addr)
        StoreInstruction addr     -> (IllinoisProtocol.store, addr)
        t                         -> error "Not expecing OtherInstruction inside handleTrace!"
    -- Run the protocol function and update internal state
    (newPState, newCache, newMemory, newBus) = case pstate of
        Nothing                           -> protocolFunction Nothing address cache mem eventBus pid
        Just (IllinoisProtocol illinoisState) -> protocolFunction (Just illinoisState) address cache mem eventBus pid
        x                                 -> error $ "handleTrace does not support state " ++ show x
    -- Design decision to NOT update the cache here since we'd have to check for equality through entire array
    -- Run a tick for the cache and memory
    elapsedCache  = Cache.elapse newCache 
    elapsedMemory = Memory.elapse newMemory
    -- Update internal state and return values
    hasConsumedTrace = isDone newPState

    -- Update statistics: 
    -- IF we receive a LOAD or STORE and we are in a NOTHING stage we must have 
    -- received a new load/store instruction. Update our statistics
    statsWithLoadStore = if isNothing pstate then addOneStatsLoadStoreCycle stats else stats
    -- If we in in this state, we MUST be idling waiting for cache
    statsWithIdleCycle = addOneStatsIdleCycle statsWithLoadStore
    -- If our state is nothing, check with the cache if we have missed 
    statsWithMissRate  = if isNothing pstate then 
            case Cache.busGetBlockState address cache of 
                Nothing -> addOneStatsMissCount statsWithIdleCycle
                _ -> statsWithIdleCycle
            else statsWithIdleCycle
    finalpstate = if hasConsumedTrace then Nothing else Just $ IllinoisProtocol newPState 
    newProcessor  = Processor pid Illinois elapsedCache statsWithMissRate cycles finalpstate elapsedMemory



-- | Runs one compute cycle from the number of cycles left to do computation
processOneComputeCycle :: Processor -> (Processor, Bool)
processOneComputeCycle (Processor pid protocol cache stats cycles pstate mem) = (newProcessor, isdone) where
    newProcessor = Processor pid protocol cache (addOneStatsComputeCycle stats) (max (cycles - 1) 0) pstate mem
    isdone = getCyclesToCompute newProcessor == 0

addOneStatsComputeCycle :: ProcessorStatistics -> ProcessorStatistics
addOneStatsComputeCycle (ProcessorStatistics compute loadstore idle misscount pid) = ProcessorStatistics (compute + 1) loadstore idle misscount pid

addOneStatsLoadStoreCycle :: ProcessorStatistics -> ProcessorStatistics
addOneStatsLoadStoreCycle (ProcessorStatistics compute loadstore idle misscount pid) = ProcessorStatistics compute (loadstore + 1) idle misscount pid

addOneStatsIdleCycle :: ProcessorStatistics -> ProcessorStatistics
addOneStatsIdleCycle (ProcessorStatistics compute loadstore idle misscount pid) = ProcessorStatistics compute loadstore (idle + 1) misscount pid

addOneStatsMissCount :: ProcessorStatistics -> ProcessorStatistics
addOneStatsMissCount (ProcessorStatistics compute loadstore idle misscount pid) = ProcessorStatistics compute loadstore idle (misscount + 1) pid