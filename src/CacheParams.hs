module CacheParams (create) where

import Definitions

create :: CacheSize -> Associativity -> BlockSize -> CacheParams
create cacheSize associativity blockSize = CacheParams cacheSize associativity blockSize numCacheSets where 
    numCacheSets = cacheSize `div` (associativity * blockSize)
