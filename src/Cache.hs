module Cache (Cache, create, isCached, read, write, canAllocate, allocate, evict) where

import CacheParams (CacheParams)
import qualified CacheParams
import CacheSet (CacheSet)
import qualified CacheSet
import CacheBlock (BlockState (M, E, S, I, C, SC, D, SD))
import Data.Array as Array
import Definitions
import qualified MemoryAddress
import Prelude hiding (read)

data Cache = Cache {
    getCacheParams :: CacheParams,
    getCacheSets :: Array Int CacheSet
} deriving (Show)

main = print $ Cache.canAllocate 0x00000001 $ Cache.allocate E 0x00000000 $ Cache.allocate E 0x00000001 $ Cache.create 64 2 8

-- |Creates an empty cache with the specified cache size, associativity, and block size.
--  Returns the empty cache on successful execution.
create :: CacheSize -> Associativity -> BlockSize -> Cache
create cacheSize associativity blockSize = Cache cacheParams cacheStructure where
    cacheParams = CacheParams.create cacheSize associativity blockSize
    cacheStructure = Array.array (0, lastIndex) [(i, cacheSets) | i <- [0..lastIndex]] where
        lastIndex = (CacheParams.getNumCacheSets cacheParams) - 1
        cacheSets = CacheSet.create associativity blockSize

-- |Checks whether the specified memory address is cached.
--  Returns True if the memory address is cached, False otherwise.
isCached :: MemoryAddress -> Cache -> Bool
isCached memoryAddress cache = CacheSet.hasTag blockTag cacheSet where
    (blockTag, setIndex, _) = MemoryAddress.parse cacheParams memoryAddress where
        cacheParams = getCacheParams cache

    cacheSet = (getCacheSets cache)!setIndex

-- |Attempts to do a cache read of a memory address on the specified cache.
--  Returns the read hit flag and the renewed cache.
read :: MemoryAddress -> Cache -> (IsReadHit, Cache)
read memoryAddress cache = (isReadHit, newCache) where
    (blockTag, setIndex, _) = MemoryAddress.parse cacheParams memoryAddress where
        cacheParams = getCacheParams cache

    isReadHit = CacheSet.hasTag blockTag $ cacheSets!setIndex where
        cacheSets = getCacheSets cache

    newCache =
        if isReadHit
            then cache -- TODO: LRU --
            else cache

-- |Attempts to do a cache write of a memory address to the specified cache.
--  Returns the write hit flag and the renewed cache.
write :: MemoryAddress -> Cache -> (IsWriteHit, Cache)
write memoryAddress cache = (isWriteHit, newCache) where
    (blockTag, setIndex, _) = MemoryAddress.parse cacheParams memoryAddress where
        cacheParams = getCacheParams cache

    isWriteHit = CacheSet.hasTag blockTag $ cacheSets!setIndex where
        cacheSets = getCacheSets cache

    newCache =
        if isWriteHit
            then cache -- TODO: LRU --
            else cache

canAllocate :: MemoryAddress -> Cache -> Bool
canAllocate memoryAddress cache = hasAvailableCacheBlock where
    (_, setIndex, _) = MemoryAddress.parse cacheParams memoryAddress where
        cacheParams = getCacheParams cache

    hasAvailableCacheBlock = CacheSet.canAllocate $ cacheSets!setIndex where
        cacheSets = getCacheSets cache

allocate :: BlockState -> MemoryAddress -> Cache -> Cache
allocate blockState memoryAddress cache = newCache where
    (blockTag, setIndex, offset) = MemoryAddress.parse cacheParams memoryAddress where
        cacheParams = getCacheParams cache

    newCache = Cache oldCacheParams newCacheSets where
        oldCacheParams = getCacheParams cache

        newCacheSets = oldCacheSets//[(i, newCacheSet) | i <- [setIndex]] where
            oldCacheSets = getCacheSets cache

            newCacheSet = CacheSet.allocate blockState blockTag offset memoryAddress $ cacheSets!setIndex where
                cacheSets = getCacheSets cache

evict :: MemoryAddress -> Cache -> Cache
evict memoryAddress cache = newCache where
    (_, setIndex, _) = MemoryAddress.parse cacheParams memoryAddress where
        cacheParams = getCacheParams cache

    newCache = Cache oldCacheParams newCacheSets where
        oldCacheParams = getCacheParams cache

        newCacheSets = oldCacheSets//[(i, newCacheSet) | i <- [setIndex]] where
            oldCacheSets = getCacheSets cache

            newCacheSet = CacheSet.evict $ oldCacheSets!setIndex
