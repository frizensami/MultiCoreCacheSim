# cs4223-as2

## Introduction
This is a trace-based simulator written in Haskell for running multi-core cache simulations. One trace for each core will be provided, and statistics will be printed out once the simulation is complete. This simulation supports the MESI and Dragon cache coherence protocols. 

## Installation
1. Run `stack setup`
2. Run `stack install`
3. Run `stack build`
4. Run `stack test` to see test case output
5. Run `stack exec -- cs4223-as2-exe <protocol:MESI/Dragon> <input_file> <cache_size_in_bytes> <associativity> <block_size_in_bytes>`


