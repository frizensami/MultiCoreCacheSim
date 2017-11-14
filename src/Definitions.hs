module Definitions where

import Data.Int

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
type MemoryAddress = Int32
type IsBusy = Bool
type IsReadHit = Bool
type IsWriteHit = Bool
type NumCacheSets = Int
type CacheBusyCycles = Int
type Tag = Int
type SetIndex = Int
type Offset = Int
