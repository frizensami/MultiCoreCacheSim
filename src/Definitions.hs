module Definitions where

import Data.Int

data Protocol = MESI | Dragon | Illinois deriving (Show, Read)
type ProtocolInput = String
type Filename = String
type CacheSize = Int
type Associativity = Int
type BlockSize = Int
type ProcessorCompleteStatus = Bool
type ProcessorsTraces = [[String]]
type StatsReport = String
type Message = Int
type MemoryAddress = Int32
type IsBusy = Bool
type IsReadHit = Bool
type IsWriteHit = Bool
type NumCacheSets = Int
type BlockTag = Int
type SetIndex = Int
type Offset = Int
type NumCycles = Int
