module CacheBlock (BlockState (M, E, S, I, C, SC, D, SD), CacheBlock, create, allocate, evict, isValid, hasTag, setDirty) where

import Data.Array as Array
import Definitions

data BlockState = M | E | S | I | C | SC | D | SD deriving (Show, Eq)

data CacheBlock = CacheBlock {
    getBlockState :: BlockState,
    isBlockDirty :: Bool,
    getBlockTag :: BlockTag,
    getCachedAddresses :: Array Int MemoryAddress
} deriving (Show)

main = print $ allocate E 123 4 0x00000008 $ create 16

-- |Creates an empty cache block with the specified block size.
--  Returns the empty cache block.
create :: BlockSize -> CacheBlock
create blockSize = CacheBlock I False 0 cachedAddresses where
    numAddresses = blockSize `div` 4
    cachedAddresses = Array.array (0, numAddresses - 1) [(i, 0) | i <- [0..numAddresses - 1]]

allocate :: BlockState -> BlockTag -> Offset -> MemoryAddress -> CacheBlock -> CacheBlock
allocate blockState blockTag offset memoryAddress cacheBlock = newCacheBlock where
    newCachedAddresses = oldCachedAddresses//[(i, headMemoryAddress + fromIntegral i * 4) | i <- [0..lastIndex]] where
        oldCachedAddresses = getCachedAddresses cacheBlock
        headMemoryAddress = memoryAddress - fromIntegral offset
        lastIndex = (length oldCachedAddresses) - 1

    newCacheBlock = CacheBlock blockState False blockTag newCachedAddresses

evict :: CacheBlock -> CacheBlock
evict cacheBlock = newCacheBlock where
    newCachedAddresses = oldCachedAddresses//[(i, 0) | i <- [0..lastIndex]] where
        oldCachedAddresses = getCachedAddresses cacheBlock
        lastIndex = (length oldCachedAddresses) - 1

    newCacheBlock = CacheBlock I False 0 newCachedAddresses

-- |Checks whether this cache block is valid (state other than I).
--  Returns True if this cache block is valid, False otherwise.
isValid :: CacheBlock -> Bool
isValid cacheBlock = blockState /= I where
    blockState = getBlockState cacheBlock

-- |Checks whether this cache block has the specified tag.
--  Returns True if this cache block has the specified tag, False otherwise.
hasTag :: BlockTag -> CacheBlock -> Bool
hasTag expectedTag cacheBlock = isTagFound where
    isTagFound = blockTag == expectedTag where
        blockTag = getBlockTag cacheBlock

setDirty :: CacheBlock -> CacheBlock
setDirty cacheBlock = newCacheBlock where
    newCacheBlock = CacheBlock oldBlockState newIsBlockDirty oldBlockTag oldCachedAddresses where
        oldBlockState = getBlockState cacheBlock
        newIsBlockDirty = True
        oldBlockTag = getBlockTag cacheBlock
        oldCachedAddresses = getCachedAddresses cacheBlock
