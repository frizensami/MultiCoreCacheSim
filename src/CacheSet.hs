module CacheSet (create, allocate, evict, hasTag, setDirty) where

import qualified CacheBlock
import Definitions

main = print $ hasTag 123 $ allocate E 123 0x00000003 $ create 2 16

-- |Creates an empty cache set with the specified associativity and block size.
--  Returns the empty cache set.
create :: Associativity -> BlockSize -> CacheSet
create associativity blockSize = CacheSet cacheBlocks where 
    cacheBlocks = replicate associativity $ CacheBlock.create blockSize

-- |Allocates a cache block with given state/tag on the first available block in the specified cache set.
--  Returns the renewed cache set.
allocate :: BlockState -> BlockTag -> MemoryAddress -> CacheSet -> CacheSet
allocate blockState blockTag memoryAddress cacheSet = CacheSet newCacheBlocks where 
    oldCacheBlocks = cacheBlocks cacheSet
    newCacheBlocks = allocateFirstCacheBlock blockState blockTag memoryAddress oldCacheBlocks

allocateFirstCacheBlock :: BlockState -> BlockTag -> MemoryAddress -> [CacheBlock] -> [CacheBlock]
allocateFirstCacheBlock blockState blockTag memoryAddress [] = []
allocateFirstCacheBlock blockState blockTag memoryAddress (x:xs) 
    | not $ CacheBlock.isValid x    = xs ++ newCacheBlock
    | otherwise                     = (:[]) x ++ recurrence where 
        newCacheBlock = (:[]) $ CacheBlock.allocate blockState blockTag memoryAddress x
        recurrence = allocateFirstCacheBlock blockState blockTag memoryAddress xs

-- |Evicts the first cache block within the specified cache set.
--  Returns the renewed cache set.
evict :: CacheSet -> CacheSet
evict cacheSet = CacheSet $ evictFirstCacheBlock $ cacheBlocks cacheSet

evictFirstCacheBlock :: [CacheBlock] -> [CacheBlock]
evictFirstCacheBlock (x:xs) = xs ++ ((:[]) $ CacheBlock.evict x)

-- |Finds a tag in a valid block within the specified cache set.
--  Returns a boolean indicating whether the tag was found.
hasTag :: BlockTag -> CacheSet -> Bool
hasTag expectedTag cacheSet = recursivelyHasTag expectedTag (cacheBlocks cacheSet)

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
    newCacheBlocks = recursivelySetDirty blockTag (cacheBlocks cacheSet)

-- |Recursively traverse the given list of cache blocks and set the block with the specified block tag as dirty.
--  Returns the new list of cache blocks.
recursivelySetDirty :: BlockTag -> [CacheBlock] -> [CacheBlock]
recursivelySetDirty blockTag [] = []
recursivelySetDirty blockTag (x:xs) 
    | (CacheBlock.isValid x) && (CacheBlock.hasTag blockTag x)  = (:[]) newCacheBlock ++ recurrence 
    | otherwise                                                 = (:[]) x ++ recurrence where 
        newCacheBlock = CacheBlock.setDirty x
        recurrence = recursivelySetDirty blockTag xs
