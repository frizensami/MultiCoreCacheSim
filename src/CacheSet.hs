module CacheSet (create, allocate, findTag, setDirty) where

import qualified CacheBlock
import Definitions

main = print $ setDirty 0 $ create 2 16

-- |Creates an empty cache set with the specified associativity and block size.
--  Returns the empty cache set.
create :: Associativity -> BlockSize -> CacheSet
create associativity blockSize = CacheSet cacheBlocks where 
    cacheBlocks = replicate associativity $ CacheBlock.create blockSize

-- |Allocates a cache block with the given block tag and memory address using the LRU policy.
--  Returns the renewed cache set.
allocate :: BlockState -> BlockTag -> MemoryAddress -> CacheSet -> CacheSet
allocate blockState blockTag memoryAddress cacheSet = CacheSet newCacheBlocks where 
    oldCacheBlocks = cacheBlocks cacheSet
    newCacheBlocks = recursivelyAllocateAt (length oldCacheBlocks - 1) blockState blockTag memoryAddress (evict $ cacheBlocks cacheSet) 0

evict :: [CacheBlock] -> [CacheBlock]
evict (x:xs) = xs ++ ((:[]) $ CacheBlock.evict x)

-- |Finds a tag in a valid block within the specified cache set.
--  Returns a boolean indicating whether the tag was found.
findTag :: BlockTag -> CacheSet -> Bool
findTag expectedTag cacheSet = recursivelyFindTag expectedTag (cacheBlocks cacheSet) 0

-- |Sets the cache block with the given block tag as dirty.
--  Returns the renewed cache set.
setDirty :: BlockTag -> CacheSet -> CacheSet
setDirty blockTag cacheSet = CacheSet newCacheBlocks where 
    newCacheBlocks = recursivelySetDirty blockTag (cacheBlocks cacheSet) 0

recursivelyAllocateAt :: Int -> BlockState -> BlockTag -> MemoryAddress -> [CacheBlock] -> Int -> [CacheBlock]
recursivelyAllocateAt allocateBlockId blockState blockTag memoryAddress cacheBlocks currentBlockId
    | currentBlockId == (length cacheBlocks)    = []
    | currentBlockId == allocateBlockId         = (:[]) newCacheBlock ++ recurrence
    | otherwise                                 = (:[]) oldCacheBlock ++ recurrence where 
        oldCacheBlock = cacheBlocks!!currentBlockId
        newCacheBlock = CacheBlock.allocate blockState blockTag memoryAddress oldCacheBlock
        recurrence = recursivelyAllocateAt allocateBlockId blockState blockTag memoryAddress cacheBlocks (currentBlockId + 1)

-- |Recursively finds a tag in each cache block within the specified cache set.
--  Returns a boolean indicating whether the tag was found in any valid cache block.
recursivelyFindTag :: BlockTag -> [CacheBlock] -> Int -> Bool
recursivelyFindTag expectedTag cacheBlocks currentBlockId
    | currentBlockId == (length cacheBlocks)    = False
    | otherwise                                 = cacheBlockHasTag || recurrence where 
        currentBlock = cacheBlocks!!currentBlockId
        cacheBlockHasTag = (CacheBlock.isValid currentBlock) && (CacheBlock.hasTag expectedTag currentBlock)

        recurrence = recursivelyFindTag expectedTag cacheBlocks (currentBlockId + 1)

-- |Recursively traverse the given list of cache blocks and set the block with the specified block tag as dirty.
--  Returns the new list of cache blocks.
recursivelySetDirty :: BlockTag -> [CacheBlock] -> Int -> [CacheBlock]
recursivelySetDirty blockTag cacheBlocks currentBlockId
    | currentBlockId == (length cacheBlocks)    = []
    | otherwise                                 = (:[]) newCacheBlock ++ recurrence where 
        currentBlock = cacheBlocks!!currentBlockId
        newCacheBlock = 
            if (CacheBlock.isValid currentBlock) && (CacheBlock.hasTag blockTag currentBlock)
                then CacheBlock.setDirty currentBlock
                else currentBlock

        recurrence = recursivelySetDirty blockTag cacheBlocks (currentBlockId + 1)
