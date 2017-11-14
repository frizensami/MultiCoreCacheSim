module CacheParams (CacheParams, cacheSize, associativity, blockSize, numCacheSets, createCacheParams) where

import Definitions

data CacheParams = CacheParams {
    cacheSize :: CacheSize, 
    associativity :: Associativity, 
    blockSize :: BlockSize, 
    numCacheSets :: NumCacheSets
} deriving (Show)

createCacheParams :: CacheSize -> Associativity -> BlockSize -> CacheParams
createCacheParams cacheSize associativity blockSize = CacheParams cacheSize associativity blockSize numCacheSets where 
    numCacheSets = cacheSize `div` (associativity * blockSize)