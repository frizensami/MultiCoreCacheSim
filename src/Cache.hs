module Cache (createCache, cacheRead, cacheWrite) where

import CacheParams
import CacheSet
import Data.Array as Array
import Definitions
import qualified MemoryAddress

data Cache = Cache {
    cacheParams :: CacheParams, 
    protocol :: Protocol, 
    cacheBusyCycles :: CacheBusyCycles, 
    cacheStructure :: Array Int CacheSet
} deriving (Show)

main = print $ cacheRead 0x00000001 $ createCache 1024 2 8 MESI

-- |Creates an empty cache with the specified cache size, associativity, and block size.
--  Returns the empty cache on successful execution.
createCache :: CacheSize -> Associativity -> BlockSize -> Protocol -> Cache
createCache cacheSize associativity blockSize protocol = Cache cacheParams protocol cacheBusyCycles cacheStructure where 
    cacheParams = createCacheParams cacheSize associativity blockSize
    cacheBusyCycles = 0
    cacheStructure = Array.array (0, cacheSetMaxIndex) [(i, cacheSets) | i <- [0..cacheSetMaxIndex]] where 
        cacheSetMaxIndex = (numCacheSets cacheParams) - 1
        cacheSets = createCacheSet associativity blockSize

-- |Attempts to do a cache read of a memory address on the specified cache.
--  Returns the cache busy flag, read hit flag, and the renewed cache.
cacheRead :: MemoryAddress -> Cache -> (IsBusy, IsReadHit, Cache)
cacheRead memoryAddress cache = (isBusy, isReadHit, newCache) where 
    isBusy = cacheBusyCycles cache /= 0

    isReadHit = 
        if isBusy
            then False
            else cacheSetFindTag expectedTag cacheSet where 
                (expectedTag, setIndex, offset) = MemoryAddress.parse (cacheParams cache) memoryAddress
                cacheSet = (cacheStructure cache)!setIndex

    newCache = 
        if isBusy
            then elapseOneCycle cache
            else cache -- TODO: Provide renewed cache --

-- |Attempts to do a cache write of a memory address to the specified cache.
--  Returns the cache busy flag, read hit flag, and the renewed cache.
cacheWrite :: MemoryAddress -> Cache -> (IsBusy, IsWriteHit, Cache)
cacheWrite memoryAddress cache = (isBusy, isWriteHit, newCache) where
    isBusy = cacheBusyCycles cache /= 0

    isWriteHit = 
        if isBusy 
            then False
            else cacheSetFindTag expectedTag cacheSet where 
                (expectedTag, setIndex, offset) = MemoryAddress.parse (cacheParams cache) memoryAddress
                cacheSet = (cacheStructure cache)!setIndex

    newCache = 
        if isBusy 
            then elapseOneCycle cache
            else cache -- TODO: Provide renewed cache --

-- |Reduces the remaining busy cycles of the specified cache by 1 if the cache is busy.
--  Returns the renewed cache.
elapseOneCycle :: Cache -> Cache
elapseOneCycle cache = Cache (cacheParams cache) (protocol cache) newCacheBusyCycles (cacheStructure cache) where 
    newCacheBusyCycles = max ((cacheBusyCycles cache) - 1) 0