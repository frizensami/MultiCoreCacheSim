module Definitions where

import Data.Int
import Data.Array as Array

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
type BlockTag = Int
type SetIndex = Int
type Offset = Int

data CacheParams = CacheParams {
    cacheSize :: CacheSize, 
    associativity :: Associativity, 
    blockSize :: BlockSize, 
    numCacheSets :: NumCacheSets
} deriving (Show)

data Cache = Cache {
    cacheParams :: CacheParams, 
    cacheStructure :: Array Int CacheSet
} deriving (Show)

data CacheSet = CacheSet {
    cacheBlocks :: [CacheBlock]
} deriving (Show)

data BlockState = M | E | S | I | C | SC | D | SD deriving (Show, Eq)

data CacheBlock = CacheBlock {
    blockState :: BlockState, 
    isBlockDirty :: Bool, 
    blockTag :: BlockTag, 
    cachedAddresses :: Array Int MemoryAddress
} deriving (Show)

data Memory = Memory {
    busyCycles :: NumCycles, 
    readingAddress :: Maybe MemoryAddress, 
    writingAddress :: Maybe MemoryAddress
} deriving (Show)
