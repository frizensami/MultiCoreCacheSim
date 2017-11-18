module CacheParams (CacheParams, getCacheSize, getAssociativity, getBlockSize, getNumCacheSets, create) where

import Definitions

data CacheParams = CacheParams {
    getCacheSize :: CacheSize,
    getAssociativity :: Associativity,
    getBlockSize :: BlockSize,
    getNumCacheSets :: NumCacheSets
} deriving (Show)

create :: CacheSize -> Associativity -> BlockSize -> CacheParams
create cacheSize associativity blockSize = CacheParams cacheSize associativity blockSize numCacheSets where
    numCacheSets = cacheSize `div` (associativity * blockSize)
