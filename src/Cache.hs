module Cache (create, isCached, read, write) where

import qualified CacheParams
import qualified CacheSet
import Data.Array as Array
import Definitions
import qualified MemoryAddress
import Prelude hiding (read, write)

main = print $ Cache.allocate E 0x00000001 $ Cache.create 1024 2 8

-- |Creates an empty cache with the specified cache size, associativity, and block size.
--  Returns the empty cache on successful execution.
create :: CacheSize -> Associativity -> BlockSize -> Cache
create cacheSize associativity blockSize = Cache cacheParams cacheStructure where 
    cacheParams = CacheParams.create cacheSize associativity blockSize
    cacheStructure = Array.array (0, cacheSetMaxIndex) [(i, cacheSets) | i <- [0..cacheSetMaxIndex]] where 
        cacheSetMaxIndex = (numCacheSets cacheParams) - 1
        cacheSets = CacheSet.create associativity blockSize

-- |Checks whether the specified memory address is cached.
--  Returns True if the memory address is cached, False otherwise.
isCached :: MemoryAddress -> Cache -> Bool
isCached memoryAddress cache = CacheSet.findTag blockTag cacheSet where 
    (blockTag, setIndex, offset) = MemoryAddress.parse (cacheParams cache) memoryAddress
    cacheSet = (cacheStructure cache)!setIndex

-- |Attempts to do a cache read of a memory address on the specified cache.
--  Returns the read hit flag and the renewed cache.
read :: MemoryAddress -> Cache -> (IsReadHit, Cache)
read memoryAddress cache = (isReadHit, newCache) where 
    (blockTag, setIndex, offset) = MemoryAddress.parse (cacheParams cache) memoryAddress

    cacheSets = cacheStructure cache
    isReadHit = CacheSet.findTag blockTag $ cacheSets!setIndex

    newCache = 
        if isReadHit
            then cache -- TODO: LRU --
            else cache

-- |Attempts to do a cache write of a memory address to the specified cache.
--  Returns the write hit flag and the renewed cache.
write :: MemoryAddress -> Cache -> (IsWriteHit, Cache)
write memoryAddress cache = (isWriteHit, newCache) where 
    (blockTag, setIndex, offset) = MemoryAddress.parse (cacheParams cache) memoryAddress

    cacheSets = cacheStructure cache
    isWriteHit = CacheSet.findTag blockTag $ cacheSets!setIndex

    newCache = 
        if isWriteHit 
            then cache -- TODO: LRU --
            else cache

allocate :: BlockState -> MemoryAddress -> Cache -> Cache
allocate blockState memoryAddress cache = newCache where 
    (blockTag, setIndex, offset) = MemoryAddress.parse (cacheParams cache) memoryAddress

    cacheSets = cacheStructure cache
    newCacheSet = CacheSet.allocate blockState blockTag memoryAddress (cacheSets!setIndex)
    newCache = Cache (cacheParams cache) (cacheSets//[(i, newCacheSet) | i <- [setIndex]])
