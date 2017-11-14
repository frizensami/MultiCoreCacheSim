module Cache (createCache, cacheRead, cacheWrite) where

import CacheSet
import Data.Array as Array
import Definitions
import MemoryAddress

data Cache = Cache {
    cacheSize :: CacheSize, 
    associativity :: Associativity, 
    blockSize :: BlockSize, 
    numCacheSets :: NumCacheSets, 
    cacheSets :: Array Int CacheSet
} deriving (Show)

main = print(createCache 1024 2 8)

-- |Creates an empty cache with the specified cache size, associativity, and block size.
--  Returns the empty cache on successful execution.
createCache :: CacheSize -> Associativity -> BlockSize -> Cache
createCache cacheSize associativity blockSize = Cache cacheSize associativity blockSize numCacheSets cacheSets where 
    numCacheSets = cacheSize `div` (associativity * blockSize)
    cacheSets = Array.array (0, numCacheSets - 1) [(i, createCacheSet associativity blockSize) | i <- [0..numCacheSets - 1]]

-- |Attempts to do a cache read of a memory address on the specified cache.
--  Returns the cache busy flag, read hit flag, and the renewed cache.
cacheRead :: MemoryAddress -> Cache -> (IsBusy, IsReadHit, Cache)
cacheRead memoryAddress cache = (isBusy, isReadHit, newCache) where 
    isBusy = False -- TODO: Evaluate whether cache is busy --

    (expectedTag, setIndex, offset) = MemoryAddress.parse (blockSize cache) (numCacheSets cache) memoryAddress

    cacheSet = (cacheSets cache)!setIndex

    isReadHit = cacheSetFindTag expectedTag cacheSet

    newCache = cache -- TODO: Return modified cache --

-- |Attempts to do a cache write of a memory address to the specified cache.
--  Returns the cache busy flag, read hit flag, and the renewed cache.
cacheWrite :: MemoryAddress -> Cache -> (IsBusy, IsWriteHit, Cache)
cacheWrite memoryAddress cache = (isBusy, isWriteHit, newCache) where
    isBusy = False -- TODO: Evaluate whether cache is busy --

    (expectedTag, setIndex, offset) = MemoryAddress.parse (blockSize cache) (numCacheSets cache) memoryAddress

    cacheSet = (cacheSets cache)!setIndex

    isWriteHit = cacheSetFindTag expectedTag cacheSet

    newCache = cache -- TODO: Return modified cache --