module CacheSet (CacheSet, createCacheSet, cacheSetFindTag) where

import CacheBlock
import Definitions

data CacheSet = CacheSet {
    cacheBlocks :: [CacheBlock]
} deriving (Show)

-- |Creates an empty cache set with the specified associativity and block size.
--  Returns the empty cache set.
createCacheSet :: Associativity -> BlockSize -> CacheSet
createCacheSet associativity blockSize = CacheSet cacheBlocks where 
    cacheBlocks = replicate associativity $ createCacheBlock blockSize

-- |Finds a tag in a valid block within the specified cache set.
--  Returns a boolean indicating whether the tag was found.
cacheSetFindTag :: Tag -> CacheSet -> Bool
cacheSetFindTag tag cacheSet = recursivelyFindTag tag (cacheBlocks cacheSet) 0

-- |Recursively finds a tag in each cache block within the specified cache set.
--  Returns a boolean indicating whether the tag was found in any valid cache block.
recursivelyFindTag :: Tag -> [CacheBlock] -> Int -> Bool
recursivelyFindTag tag cacheBlocks currentBlockId
    | currentBlockId == 0   = cacheBlockFindTag tag (cacheBlocks!!currentBlockId)
    | otherwise             = (cacheBlockFindTag tag (cacheBlocks!!currentBlockId)) || recursivelyFindTag tag cacheBlocks (currentBlockId - 1)