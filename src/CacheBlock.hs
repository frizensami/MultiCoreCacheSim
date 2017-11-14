module CacheBlock (CacheBlock, createCacheBlock, cacheBlockFindTag) where 

import Data.Array as Array
import Definitions

data CacheBlock = CacheBlock {
    isBlockValid :: Bool, 
    isBlockDirty :: Bool, 
    blockTag :: Tag, 
    cachedAddresses :: Array Int MemoryAddress
} deriving (Show)

createCacheBlock :: BlockSize -> CacheBlock
createCacheBlock blockSize = CacheBlock False False 0 cachedAddresses where 
    numAddresses = blockSize `div` 8
    cachedAddresses = Array.array (0, numAddresses - 1) [(i, 0) | i <- [0..numAddresses - 1]]

cacheBlockFindTag :: Tag -> CacheBlock -> Bool
cacheBlockFindTag tag cacheBlock = isTagFound where 
    isTagFound = (isBlockValid cacheBlock) && (blockTag cacheBlock == tag)