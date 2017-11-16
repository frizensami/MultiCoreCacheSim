module Definitions where

data Protocol = MESI | Dragon deriving (Show, Read)
type ProtocolInput = String
type Filename = String
type CacheSize = Int
type Associativity = Int
type BlockSize = Int
type ProcessorCompleteStatus = Bool
type ProcessorsTraces = [[String]]
type StatsReport = String
type Message = Int
type Address = Int
type NumCycles = Int

-- Stub definitions
type Cache = String