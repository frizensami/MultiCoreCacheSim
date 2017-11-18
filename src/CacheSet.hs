module CacheSet (CacheSet, create, canAllocate, allocate, evict, hasTag, setDirty) where

import CacheBlock (BlockState (M, E, S, I, C, SC, D, SD), CacheBlock)
import qualified CacheBlock
import Definitions

data CacheSet = CacheSet {
    getCacheBlocks :: [CacheBlock]
} deriving (Show)

main = print $ hasTag 123 $ allocate E 123 4 0x00000008 $ create 2 16

-- |Creates an empty cache set with the specified associativity and block size.
--  Returns the empty cache set.
create :: Associativity -> BlockSize -> CacheSet
create associativity blockSize = CacheSet cacheBlocks where
    cacheBlocks = replicate associativity $ CacheBlock.create blockSize

canAllocate :: CacheSet -> Bool
canAllocate cacheSet = recursivelyCanAllocate $ getCacheBlocks cacheSet

recursivelyCanAllocate :: [CacheBlock] -> Bool
recursivelyCanAllocate [] = False
recursivelyCanAllocate (x:xs) = isCacheBlockEmpty || recurrence where
    isCacheBlockEmpty = not $ CacheBlock.isValid x
    recurrence = recursivelyCanAllocate xs

-- |Allocates a cache block with given state/tag on the first available block in the specified cache set.
--  Returns the renewed cache set.
allocate :: BlockState -> BlockTag -> Offset -> MemoryAddress -> CacheSet -> CacheSet
allocate blockState blockTag offset memoryAddress cacheSet = CacheSet newCacheBlocks where
    newCacheBlocks = allocateFirstCacheBlock blockState blockTag offset memoryAddress oldCacheBlocks where
        oldCacheBlocks = getCacheBlocks cacheSet

allocateFirstCacheBlock :: BlockState -> BlockTag -> Offset -> MemoryAddress -> [CacheBlock] -> [CacheBlock]
allocateFirstCacheBlock blockState blockTag offset memoryAddress [] = []
allocateFirstCacheBlock blockState blockTag offset memoryAddress (x:xs)
    | not $ CacheBlock.isValid x    = xs ++ newCacheBlock
    | otherwise                     = (:[]) x ++ recurrence where
        newCacheBlock = (:[]) $ CacheBlock.allocate blockState blockTag offset memoryAddress x
        recurrence = allocateFirstCacheBlock blockState blockTag offset memoryAddress xs

-- |Evicts the first cache block within the specified cache set.
--  Returns the renewed cache set.
evict :: CacheSet -> CacheSet
evict cacheSet = CacheSet $ evictFirstCacheBlock $ getCacheBlocks cacheSet

evictFirstCacheBlock :: [CacheBlock] -> [CacheBlock]
evictFirstCacheBlock (x:xs) = xs ++ ((:[]) $ CacheBlock.evict x)

-- |Finds a tag in a valid block within the specified cache set.
--  Returns a boolean indicating whether the tag was found.
hasTag :: BlockTag -> CacheSet -> Bool
hasTag expectedTag cacheSet = recursivelyHasTag expectedTag cacheBlocks where
    cacheBlocks = getCacheBlocks cacheSet

-- |Recursively finds a tag in each cache block within the specified cache set.
--  Returns a boolean indicating whether the tag was found in any valid cache block.
recursivelyHasTag :: BlockTag -> [CacheBlock] -> Bool
recursivelyHasTag expectedTag [] = False
recursivelyHasTag expectedTag (x:xs) = blockTagFound || recurrence where
    blockTagFound = (CacheBlock.isValid x) && (CacheBlock.hasTag expectedTag x)
    recurrence = recursivelyHasTag expectedTag xs

-- |Sets the cache block with the given block tag as dirty.
--  Returns the renewed cache set.
setDirty :: BlockTag -> CacheSet -> CacheSet
setDirty blockTag cacheSet = CacheSet newCacheBlocks where
    newCacheBlocks = recursivelySetDirty blockTag oldCacheBlocks where
        oldCacheBlocks = getCacheBlocks cacheSet

-- |Recursively traverse the given list of cache blocks and set the block with the specified block tag as dirty.
--  Returns the new list of cache blocks.
recursivelySetDirty :: BlockTag -> [CacheBlock] -> [CacheBlock]
recursivelySetDirty blockTag [] = []
recursivelySetDirty blockTag (x:xs)
    | (CacheBlock.isValid x) && (CacheBlock.hasTag blockTag x)  = (:[]) newCacheBlock ++ recurrence
    | otherwise                                                 = (:[]) x ++ recurrence where
        newCacheBlock = CacheBlock.setDirty x
        recurrence = recursivelySetDirty blockTag xs
