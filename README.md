# cs4223-as2

[![Build Status](https://travis-ci.org/frizensami/cs4223-as2.svg?branch=master)](https://travis-ci.org/frizensami/cs4223-as2)

## Introduction
This is a trace-based simulator written in Haskell for running multi-core cache simulations. One trace for each core will be provided, and statistics will be printed out once the simulation is complete. This simulation supports the MESI and Dragon cache coherence protocols. 

## Installation
1. Run `stack setup`
2. Run `stack install`
3. Run `stack build`
4. Run `stack test` to see test case output
5. Run `stack exec -- cs4223-as2-exe <protocol:MESI/Dragon> <input_file> <cache_size_in_bytes> <associativity> <block_size_in_bytes>`
6. Or you can just do: `./run_default.sh` to test a default simulator run
7. Another alternative, do: `./run <protocol:MESI/Dragon> <input_file> <cache_size_in_bytes>` to build and run one after the other


## Developer Guide
### Main Control Flow (chronological)
Entry point into simulator. Reads arguments from command line, passes it on to `runSimulator` and prints the stats report string to the screen once `runSimulator` has returned it. <br>
```haskell
-- Main.hs
main :: IO()
```

Receives the command line arguments as strings, reads the trace files required, initializes the processors, and runs them until they are done and all traces are exhausted.
```haskell
-- SimulatorCore.hs
runSimulation :: ProtocolInput -> Filename -> CacheSize -> Associativity -> BlockSize -> IO String
```

Given a list of all processors and all traces to execute, zips them together to tie every processor together with its own list of traces to execute, and starts execution of all simulation cycles from cycle 0 to whenever all processors finish.
```haskell
-- SimulatorCore.hs
startSimulationPure :: [Processor] -> [[Trace]] -> StatsReport
```

This is a recursive function that takes in the list of processor-trace combinations, the shared event bus for them, the index of the current processor within the cycle that is being executed, and the total number of cycles completed so far. Each cycle, all the processors are run with `runOneProcessorCycle` and then finally `executeEventBus` is called.
```haskell
-- SimulatorCore.hs
runAllSimulationCycles :: [(Processor, [Trace])] -> CacheEventBus -> Int -> Int -> StatsReport
```

This runs a single cycle for a single processor, updating the processor itself, its remaining traces, and the state of the cache bus.
```haskell
-- SimulatorCore.hs
runOneProcessorCycle :: (Processor, [Trace]) -> CacheEventBus -> (Processor, [Trace], CacheEventBus)
```

### Concept of a single cycle
One cycle will occur as such:
- Phase 1 (Trace Execution). For each processor x = [1...n]
    - Input:  (Current processor, List of all traces left to execute, Bus Queue). 
    - Action: Consume a trace if ready. Otherwise, continue working on whatever is pending/waiting.
    - Output: (Updated processor, List of all traces left to execute (perhaps with one consumed), New Bus Queue).
- Phase 2 (Bus Transactions). After all processors are done with Phase 1, the first event on the bus is dequeued IF there are no pending events on the bus right now (e.g. read miss and memory is being accessed). Then, all processors respond to this bus event on this cycle if able.
- Phase 3 (Completion). After Phases 1 and 2, the cycle is complete and Phase 1 begins again.

### TODO
Processor.hs is just a stub - processor data types must be properly defined, so must be the cache structures. Also, separation of concerns must be achieved between the protocol being used (MESI/Dragon) and the rest of the structures. 

Also, tests must be implemented for these lower-level data structures (like the caches etc).


