module SimulatorCore
    ( runSimulation
    ) where

import Trace
import qualified Processor (createProcessor, runOneCycle)
import Definitions
import Control.Monad (liftM)
import Utility



-- Defining constants
num_processors = 4

-- |Given appropriate cmd line arguments, runs the entire cache simulation and results the stats results
-- Reads in all traces and kicks off the pure part of this simulation
runSimulation :: ProtocolInput -> Filename -> CacheSize -> Associativity -> BlockSize -> IO String
runSimulation protocolInput fileName cacheSize associativity blockSize = 
    do
        let 

            -- Parse the protocol
            -- Possible error with read here if bad input given
            protocol       = read protocolInput :: Protocol 
            
            -- Read the input files
            -- Create exactly 4 trace files names
            fileNames      = map (\n -> fileName ++ "_" ++ show n ++ ".data") [0..(num_processors-1)] 

            -- Create a set of strings, each will lazily read from the file as needed (IO [String]])
            fileStrings    = mapM (readFile) fileNames 

            -- Read the list of strings from the file read process into a list of list of lines
            -- Each element of the list is a list of the lines from a single file
            -- liftM applies this composed & curried function into the list monad
            fileLines      = liftM (map lines) fileStrings

            -- Map each of these lines to a trace
            -- This is now of type IO [[Trace]] where each outer list element is one processor
            -- and the inner lists are the traces for that processor
            tracesIO       = liftM ((map . map) toTraceWithError) fileLines

            -- Initialize as many processors as we need in their initial states
            newProcessor   = Processor.createProcessor protocolInput cacheSize associativity blockSize
            processorsList = replicate num_processors newProcessor

        -- Get the entire list of pure traces from the IO [[Trace]] structure
        tracesList <- tracesIO

        -- Run the pure section of the simulation (end of impure code, start of actual sim)
        let statsReport = startSimulationPure processorsList tracesList

        -- Now run one round of the simulation loop and see if a core needs another instruction
        return statsReport

-- General idea: for each processor, execute the new trace if there is one, or continue with current action. Then, propagate the bus events to ALL OTHER PROCESSORS: aka all other processors MUST RESPOND to the bus event before this processors' CYCLE is over. Then, repeat for the rest. Check when more awake whether this makes sense. Therefore, processor 1 always has priority, followed by 2...etc. Deterministic
-- Morning note: this makes sense. It's almost like each processor is running sequentially with a particular priority on each cycle, which could happen in a particularly biased computer.

-- |Runs a single cycle for a single processor. Attempts to feed it a list of traces. The processor may consume some part of the list of traces, then return a new Processor instance (the new processor state essentially), the remaining traces to be consumed, and a list of bus events generated by this processor
-- runOneProcessorCycle :: Processor -> [Trace] -> (Processor, [Trace], [Message])
-- runOneProcessorCycle processor tracexs = 

-- |Start of the pure part of the code - the IO for the traces has been unwrapped in runSimulation
startSimulationPure :: [Processor] -> [[Trace]] -> StatsReport
startSimulationPure processorsList tracesList = 
    let 
        -- processorTraceList :: [(Processor, [Trace])]
        processorTraceList = zip processorsList tracesList
        -- Create the bus queue
        --eventBus = createEventBus num_processors
        -- For each processor, we need to 
        -- 1. Attempt to run one trace
        -- 2. Send their generated messages to all other processes
        -- 3. Propagate all messages 
        report = runSimulationCycles processorTraceList 0 0
        
    in "----Simulation Complete----\n" ++ (show report)

-- |Pass in the processors and traces, tbe index of the current processor being worked on, and the number of cycles completed.
-- |Eventually the statistics report will be returned
runSimulationCycles :: [(Processor, [Trace])] -> Int -> Int -> StatsReport
runSimulationCycles processorTraceList processorIndex numCyclesCompleted = 
    -- Run this simulation for all 4 processors then increment the cycle counter
    let 
        -- Get the current processor to be operated on, and the rest of the list
        currentProcessor = processorTraceList!!processorIndex
        restOfProcessors = removeNthElement processorTraceList processorIndex

        -- Mainly this function runs the sim cycle for a single processor, modifying itself and the rest
        newProcessorTraceList = runSimulationCycleForProcessor currentProcessor restOfProcessors

        -- Define the arguments for the next recursive call
        newProcessorIndex = (processorIndex + 1) `mod` num_processors
        newNumCyclesCompleted = if newProcessorIndex == 0 then numCyclesCompleted + 1 else numCyclesCompleted
    in 
        if allProcessorsComplete processorTraceList
            then getStatsReport processorTraceList
            else runSimulationCycles newProcessorTraceList newProcessorIndex newNumCyclesCompleted

    


-- |Runs a single simulation cycle with a single processor at its heart
-- Next argument is the remaining processors 
runSimulationCycleForProcessor :: (Processor, [Trace]) -> [(Processor, [Trace])] -> [(Processor, [Trace])]
runSimulationCycleForProcessor currentProcessor restOfProcessors = 
    let 
        messages = runOneProcessorCycle currentProcessor
    in error "tbi"


-- |Attempts to feed one trace to the processor, which it can avoid consuming if it's already working on something/busy
-- Returns a new processor with updated state, the remaining traces to execute, and any new bus events to propagate
runOneProcessorCycle :: (Processor, [Trace]) -> (Processor, [Trace], [Message])
runOneProcessorCycle (processor, allTraces@(oneTrace:restOfTraces)) = 
    let (newProcessor, hasConsumedTrace, newMessages) = Processor.runOneCycle processor (Just oneTrace)
    in  (newProcessor, if hasConsumedTrace then restOfTraces else allTraces, newMessages) 
runOneProcessorCycle (processor, []) = 
    let (newProcessor, _, newMessages) = Processor.runOneCycle processor Nothing
    in  (newProcessor, [], newMessages)

allProcessorsComplete :: [(Processor, [Trace])] -> Bool
allProcessorsComplete processorTraceList = True

getStatsReport :: [(Processor, [Trace])] -> StatsReport
getStatsReport processorTraceList = "TBI"
