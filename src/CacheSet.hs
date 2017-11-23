module CacheSet (CacheSet, create, canAllocate, allocate, evictLRU, evict, hasTag, getBlockState, setBlockState) where

import CacheBlock (BlockState, CacheBlock)
import qualified CacheBlock
import Data.Maybe
import Definitions

data CacheSet = CacheSet [CacheBlock]

-- |Creates an empty cache set with the specified associativity and block size.
--  Returns the empty cache set.
create :: Associativity -> BlockSize -> CacheSet
create associativity blockSize = CacheSet cacheBlocks where
    cacheBlocks = replicate associativity $ CacheBlock.create blockSize

-- |Checks whether a block in this cache is available for allocation.
--  Returns True if there is an empty block, False otherwise.
canAllocate :: CacheSet -> Bool
canAllocate (CacheSet cacheBlocks) = recursivelyCanAllocate cacheBlocks

recursivelyCanAllocate :: [CacheBlock] -> Bool
recursivelyCanAllocate [] = False
recursivelyCanAllocate (x:xs) = isCacheBlockEmpty || recurrence where
    isCacheBlockEmpty = not $ CacheBlock.isValid x
    recurrence = recursivelyCanAllocate xs

-- |Allocates a cache block with given state/tag on the first available block in the specified cache set.
--  Returns the renewed cache set.
allocate :: BlockState -> BlockTag -> Offset -> MemoryAddress -> CacheSet -> CacheSet
allocate blockState blockTag offset memoryAddress (CacheSet oldCacheBlocks) = newCacheSet where
    newCacheSet = CacheSet newCacheBlocks where
        newCacheBlocks = allocateFirstCacheBlock blockState blockTag offset memoryAddress oldCacheBlocks

allocateFirstCacheBlock :: BlockState -> BlockTag -> Offset -> MemoryAddress -> [CacheBlock] -> [CacheBlock]
allocateFirstCacheBlock blockState blockTag offset memoryAddress [] = []
allocateFirstCacheBlock blockState blockTag offset memoryAddress (x:xs)
    | not $ CacheBlock.isValid x    = xs ++ (:[]) newCacheBlock
    | otherwise                     = x : recurrence where
        newCacheBlock = CacheBlock.allocate blockState blockTag offset memoryAddress x
        recurrence = allocateFirstCacheBlock blockState blockTag offset memoryAddress xs

-- |Evicts the first valid cache block within the specified cache set.
--  Returns the renewed cache set.
evictLRU :: CacheSet -> (BlockState, CacheSet)
evictLRU (CacheSet oldCacheBlocks) = (evictedBlockState, newCacheSet) where
    (maybeEvictedBlockState, newCacheBlocks) = recursivelyEvictLRU oldCacheBlocks
    evictedBlockState = fromJust maybeEvictedBlockState
    newCacheSet = CacheSet newCacheBlocks

recursivelyEvictLRU :: [CacheBlock] -> (Maybe BlockState, [CacheBlock])
recursivelyEvictLRU [] = (Nothing, [])
recursivelyEvictLRU (x:xs)
    | CacheBlock.isValid x  = combineEvictTuple (Just evictedBlockState, newCacheBlock) recurrence
    | otherwise             = combineEvictTuple (Nothing, x) recurrence
    where
        (evictedBlockState, newCacheBlock) = CacheBlock.evict x
        recurrence = recursivelyEvictLRU xs

-- |Evicts the cache block with the specified tag on the specified cache set.
--  Returns the renewed cache set.
evict :: BlockTag -> CacheSet -> (BlockState, CacheSet)
evict blockTag (CacheSet oldCacheBlocks) = (evictedBlockState, newCacheSet) where
    (maybeEvictedBlockState, newCacheBlocks) = recursivelyEvict blockTag oldCacheBlocks
    evictedBlockState = fromJust maybeEvictedBlockState
    newCacheSet = CacheSet newCacheBlocks

recursivelyEvict :: BlockTag -> [CacheBlock] -> (Maybe BlockState, [CacheBlock])
recursivelyEvict blockTag [] = (Nothing, [])
recursivelyEvict blockTag (x:xs)
    | CacheBlock.isValid x && CacheBlock.hasTag blockTag x  = combineEvictTuple (Just evictedBlockState, newCacheBlock) recurrence
    | otherwise                                             = combineEvictTuple (Nothing, x) recurrence
    where
        (evictedBlockState, newCacheBlock) = CacheBlock.evict x
        recurrence = recursivelyEvict blockTag xs

combineEvictTuple :: (Maybe BlockState, CacheBlock) -> (Maybe BlockState, [CacheBlock]) -> (Maybe BlockState, [CacheBlock])
combineEvictTuple (Nothing, blockX) (Nothing, blocksY) = (Nothing, blockX : blocksY)
combineEvictTuple (Just stateX, blockX) (Nothing, blocksY) = (Just stateX, blockX : blocksY)
combineEvictTuple (Nothing, blockX) (Just stateY, blocksY) = (Just stateY, blockX : blocksY)
combineEvictTuple (Just stateX, blockX) (Just stateY, blocksY) = (Just stateX, blockX : blocksY)

-- |Finds a tag in a valid block within the specified cache set.
--  Returns a boolean indicating whether the tag was found.
hasTag :: BlockTag -> CacheSet -> Bool
hasTag expectedTag (CacheSet cacheBlocks) = recursivelyHasTag expectedTag cacheBlocks

-- |Recursively finds a tag in each cache block within the specified cache set.
--  Returns a boolean indicating whether the tag was found in any valid cache block.
recursivelyHasTag :: BlockTag -> [CacheBlock] -> Bool
recursivelyHasTag expectedTag [] = False
recursivelyHasTag expectedTag (x:xs) = blockTagFound || recurrence where
    blockTagFound = (CacheBlock.isValid x) && (CacheBlock.hasTag expectedTag x)
    recurrence = recursivelyHasTag expectedTag xs

-- |Retrieves the block state corresponding to the specified block tag on the specified cache set.
--  Returns the block state if found, Nothing otherwise.
getBlockState :: BlockTag -> CacheSet -> Maybe BlockState
getBlockState blockTag (CacheSet cacheBlocks) = recursivelyGetBlockState blockTag cacheBlocks

recursivelyGetBlockState :: BlockTag -> [CacheBlock] -> Maybe BlockState
recursivelyGetBlockState blockTag [] = Nothing
recursivelyGetBlockState blockTag (x:xs)
    | (CacheBlock.isValid x) && (CacheBlock.hasTag blockTag x)  = combineMaybeBlockState (Just blockState) recurrence
    | otherwise                                                 = combineMaybeBlockState Nothing recurrence where
        blockState = CacheBlock.getBlockState x
        recurrence = recursivelyGetBlockState blockTag xs

combineMaybeBlockState :: (Maybe BlockState) -> (Maybe BlockState) -> (Maybe BlockState)
combineMaybeBlockState (Just stateX) Nothing = Just stateX
combineMaybeBlockState Nothing (Just stateY) = Just stateY
combineMaybeBlockState (Just stateX) (Just stateY) = Just stateX
combineMaybeBlockState Nothing Nothing = Nothing

-- |Sets the state of the cache block with the given block tag to the given state.
--  Returns the renewed cache set.
setBlockState :: BlockState -> BlockTag -> CacheSet -> CacheSet
setBlockState blockState blockTag (CacheSet oldCacheBlocks) = newCacheSet where
    newCacheSet = CacheSet newCacheBlocks where
        newCacheBlocks = recursivelySetBlockState blockState blockTag oldCacheBlocks

recursivelySetBlockState :: BlockState -> BlockTag -> [CacheBlock] -> [CacheBlock]
recursivelySetBlockState blockState blockTag [] = []
recursivelySetBlockState blockState blockTag (x:xs)
    | (CacheBlock.isValid x) && (CacheBlock.hasTag blockTag x)  = newCacheBlock : recurrence
    | otherwise                                                 = x : recurrence where
        newCacheBlock = CacheBlock.setBlockState blockState x
        recurrence = recursivelySetBlockState blockState blockTag xs
