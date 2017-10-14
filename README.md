# cs4223-as2

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
-- Simulator.hs
runSimulation :: ProtocolInput -> Filename -> CacheSize -> Associativity -> BlockSize -> IO String
```

### TODO
Processor.hs is just a stub - processor data types must be properly defined, so must be the cache structures. Also, separation of concerns must be achieved between the protocol being used (MESI/Dragon) and the rest of the structures. 

Also, tests must be implemented for these lower-level data structures (like the caches etc).