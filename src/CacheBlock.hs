module CacheBlock (BlockState (..), CacheBlock, create, allocate, evict, isValid, hasTag, getBlockState, setBlockState) where

import Data.Array as Array
import Definitions

data BlockState = M | E | S | I | SC | SM deriving (Show, Eq)

data CacheBlock = CacheBlock BlockState (Maybe BlockTag) (Array Int MemoryAddress)

-- |Creates an empty cache block with the specified block size.
--  Returns the empty cache block.
create :: BlockSize -> CacheBlock
create blockSize = CacheBlock I Nothing cachedAddresses where
    numAddresses = blockSize `div` 4
    cachedAddresses = Array.array (0, numAddresses - 1) [(i, 0) | i <- [0..numAddresses - 1]]

allocate :: BlockState -> BlockTag -> Offset -> MemoryAddress -> CacheBlock -> CacheBlock
allocate blockState blockTag offset memoryAddress (CacheBlock _ _ oldCachedAddresses) = newCacheBlock where
    newCacheBlock = CacheBlock blockState (Just blockTag) newCachedAddresses where
        newCachedAddresses = oldCachedAddresses//[(i, headMemoryAddress + fromIntegral i * 4) | i <- [0..lastIndex]] where
            headMemoryAddress = memoryAddress - fromIntegral offset
            lastIndex = (length oldCachedAddresses) - 1

evict :: CacheBlock -> (BlockState, CacheBlock)
evict (CacheBlock oldBlockState _ oldCachedAddresses) = (oldBlockState, newCacheBlock) where
    newCacheBlock = CacheBlock I Nothing newCachedAddresses where
        newCachedAddresses = oldCachedAddresses//[(i, 0) | i <- [0..lastIndex]] where
            lastIndex = (length oldCachedAddresses) - 1

-- |Checks whether this cache block is valid (state other than I).
--  Returns True if this cache block is valid, False otherwise.
isValid :: CacheBlock -> Bool
isValid (CacheBlock blockState _ _) = blockState /= I

-- |Checks whether this cache block has the specified tag.
--  Returns True if this cache block has the specified tag, False otherwise.
hasTag :: BlockTag -> CacheBlock -> Bool
hasTag expectedTag (CacheBlock _ blockTag _) = blockTag == Just expectedTag

getBlockState :: CacheBlock -> BlockState
getBlockState (CacheBlock blockState _ _) = blockState

setBlockState :: BlockState -> CacheBlock -> CacheBlock
setBlockState blockState (CacheBlock _ oldBlockTag oldCachedAddresses) = newCacheBlock where
    newCacheBlock = CacheBlock blockState oldBlockTag oldCachedAddresses
