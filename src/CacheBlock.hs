module CacheBlock (create, allocate, evict, isValid, hasTag, setDirty, setTag) where 

import Data.Array as Array
import Definitions

main = print $ allocate E 123 0x00000003 $ create 16

-- |Creates an empty cache block with the specified block size.
--  Returns the empty cache block.
create :: BlockSize -> CacheBlock
create blockSize = CacheBlock I False 0 cachedAddresses where 
    numAddresses = blockSize `div` 4
    cachedAddresses = Array.array (0, numAddresses - 1) [(i, 0) | i <- [0..numAddresses - 1]]

allocate :: BlockState -> BlockTag -> MemoryAddress -> CacheBlock -> CacheBlock
allocate blockState blockTag memoryAddress cacheBlock = newCacheBlock where 
    newCachedAddresses = (cachedAddresses cacheBlock)//[(i, memoryAddress) | i <- [0]] -- TODO: Properly update cached addresses sequence --
    newCacheBlock = CacheBlock blockState False blockTag newCachedAddresses

evict :: CacheBlock -> CacheBlock
evict cacheBlock = newCacheBlock where 
    oldCachedAddresses = (cachedAddresses cacheBlock)
    newCachedAddresses = oldCachedAddresses//[(i, 0) | i <- [0..(length oldCachedAddresses - 1)]]
    newCacheBlock = CacheBlock I False 0 newCachedAddresses

-- |Checks whether this cache block is valid (state other than I).
--  Returns True if this cache block is valid, False otherwise.
isValid :: CacheBlock -> Bool
isValid cacheBlock = blockState cacheBlock /= I

-- |Checks whether this cache block has the specified tag.
--  Returns True if this cache block has the specified tag, False otherwise.
hasTag :: BlockTag -> CacheBlock -> Bool
hasTag expectedTag cacheBlock = isTagFound where 
    isTagFound = blockTag cacheBlock == expectedTag

setDirty :: CacheBlock -> CacheBlock
setDirty cacheBlock = newCacheBlock where 
    newCacheBlock = CacheBlock (blockState cacheBlock) True (blockTag cacheBlock) (cachedAddresses cacheBlock)

setTag :: BlockTag -> CacheBlock -> CacheBlock
setTag newTag cacheBlock = newCacheBlock where 
    newCacheBlock = CacheBlock (blockState cacheBlock) (isBlockDirty cacheBlock) newTag (cachedAddresses cacheBlock)
