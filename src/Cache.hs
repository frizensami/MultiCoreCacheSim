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
    cacheBusyCycles :: CacheBusyCycles, 
    cacheSets :: Array Int CacheSet
} deriving (Show)

main = print $ cacheRead 0x00000001 $ createCache 1024 2 8

-- |Creates an empty cache with the specified cache size, associativity, and block size.
--  Returns the empty cache on successful execution.
createCache :: CacheSize -> Associativity -> BlockSize -> Cache
createCache cacheSize associativity blockSize = Cache cacheSize associativity blockSize numCacheSets cacheBusyCycles cacheSets where 
    numCacheSets = cacheSize `div` (associativity * blockSize)
    cacheBusyCycles = 0
    cacheSets = Array.array (0, numCacheSets - 1) [(i, createCacheSet associativity blockSize) | i <- [0..numCacheSets - 1]]

-- |Attempts to do a cache read of a memory address on the specified cache.
--  Returns the cache busy flag, read hit flag, and the renewed cache.
cacheRead :: MemoryAddress -> Cache -> (IsBusy, IsReadHit, Cache)
cacheRead memoryAddress cache = (isBusy, isReadHit, newCache) where 
    isBusy = cacheBusyCycles cache /= 0

    (expectedTag, setIndex, offset) = MemoryAddress.parse (blockSize cache) (numCacheSets cache) memoryAddress
    cacheSet = (cacheSets cache)!setIndex
    isReadHit = cacheSetFindTag expectedTag cacheSet

    newCache = elapseOneCycle cache

-- |Attempts to do a cache write of a memory address to the specified cache.
--  Returns the cache busy flag, read hit flag, and the renewed cache.
cacheWrite :: MemoryAddress -> Cache -> (IsBusy, IsWriteHit, Cache)
cacheWrite memoryAddress cache = (isBusy, isWriteHit, newCache) where
    isBusy = cacheBusyCycles cache /= 0

    (expectedTag, setIndex, offset) = MemoryAddress.parse (blockSize cache) (numCacheSets cache) memoryAddress
    cacheSet = (cacheSets cache)!setIndex
    isWriteHit = cacheSetFindTag expectedTag cacheSet

    newCache = elapseOneCycle cache

elapseOneCycle :: Cache -> Cache
elapseOneCycle cache = Cache (cacheSize cache) (associativity cache) (blockSize cache) (numCacheSets cache) newCacheBusyCycles (cacheSets cache) where 
    newCacheBusyCycles = max ((cacheBusyCycles cache) - 1) 0