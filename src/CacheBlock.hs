module CacheBlock (CacheBlock, createCacheBlock, cacheBlockFindTag) where 

import Data.Array as Array
import Definitions

data CacheBlock = CacheBlock {
    isBlockValid :: Bool, 
    isBlockDirty :: Bool, 
    blockTag :: Tag, 
    cachedAddresses :: Array Int MemoryAddress
} deriving (Show)

-- |Creates an empty cache block with the specified block size.
--  Returns the empty cache block.
createCacheBlock :: BlockSize -> CacheBlock
createCacheBlock blockSize = CacheBlock False False 0 cachedAddresses where 
    numAddresses = blockSize `div` 8
    cachedAddresses = Array.array (0, numAddresses - 1) [(i, 0) | i <- [0..numAddresses - 1]]

-- |Checks whether this cache block is valid and has the specified tag.
--  Returns True if this cache block satisfies the requirement, False otherwise.
cacheBlockFindTag :: Tag -> CacheBlock -> Bool
cacheBlockFindTag tag cacheBlock = isTagFound where 
    isTagFound = (isBlockValid cacheBlock) && (blockTag cacheBlock == tag)
