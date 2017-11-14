module Definitions where

import Data.Int

data Protocol = MESI | Dragon deriving (Show, Read)
data BlockState = M | E | S | I | C | SC | D | SD deriving (Show, Eq)
type ProtocolInput = String
type Filename = String
type CacheSize = Int
type Associativity = Int
type BlockSize = Int
type ProcessorCompleteStatus = Bool
type ProcessorsTraces = [[String]]
type StatsReport = String
type Processor = Int
type Message = Int
type MemoryAddress = Int32
type IsBusy = Bool
type IsReadHit = Bool
type IsWriteHit = Bool
type NumCacheSets = Int
type CacheBusyCycles = Int
type Tag = Int
type SetIndex = Int
type Offset = Int
