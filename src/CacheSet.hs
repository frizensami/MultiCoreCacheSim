module CacheSet (CacheSet, createCacheSet, cacheSetFindTag) where

import CacheBlock
import Definitions

data CacheSet = CacheSet {
    cacheBlocks :: [CacheBlock]
} deriving (Show)

createCacheSet :: Associativity -> BlockSize -> CacheSet
createCacheSet associativity blockSize = CacheSet cacheBlocks where 
    cacheBlocks = replicate associativity $ createCacheBlock blockSize

cacheSetFindTag :: Tag -> CacheSet -> Bool
cacheSetFindTag tag cacheSet = recursivelyFindTag tag (cacheBlocks cacheSet) 0

recursivelyFindTag :: Tag -> [CacheBlock] -> Int -> Bool
recursivelyFindTag tag cacheBlocks currentBlockId
    | currentBlockId == 0   = cacheBlockFindTag tag (cacheBlocks!!currentBlockId)
    | otherwise             = (cacheBlockFindTag tag (cacheBlocks!!currentBlockId)) || recursivelyFindTag tag cacheBlocks (currentBlockId - 1)