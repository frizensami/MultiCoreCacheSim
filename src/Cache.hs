module Cache (createCache, cacheRead, cacheWrite, cacheInvalidate) where

import Data.Map (Map)
import qualified Data.Map as Map
import Definitions

data Cache = Cache {
    cacheSize :: Int, 
    associativity :: Int, 
    blockSize :: Int, 
    cacheContent :: Map (Int, Int) CacheBlock
} deriving (Show)

data CacheBlock = CacheBlock {
    isBlockValid :: Bool, 
    isBlockDirty :: Bool, 
    blockContent :: [Int]
} deriving (Show)

type MemoryAddress = Int
type IsReadHit = Bool
type IsWriteHit = Bool

main = print (
    cacheInvalidate (
        fst (
            cacheRead (createCache 64 2 4) 0x00000001)
        ) 
    0x00000001)

{-  Creates an empty cache with the specified cache size, associativity, and block size.
    Returns the empty cache on successful execution.
-}
createCache :: CacheSize -> Associativity -> BlockSize -> Cache
createCache cacheSize associativity blockSize = Cache cacheSize associativity blockSize Map.empty

{-  Attempts to do a cache read of a memory address on the specified cache.
    Returns the new cache containing the memory address and whether the cache read was a hit.
-}
cacheRead :: Cache -> MemoryAddress -> (Cache, IsReadHit)
cacheRead cache memoryAddress = (newCache, isReadHit) where
    numCacheBlocks = (cacheSize cache) `div` (blockSize cache)
    cacheIndex = memoryAddress `mod` numCacheBlocks
    cacheTag = memoryAddress `div` numCacheBlocks
    isReadHit = Map.member (cacheIndex, cacheTag) (cacheContent cache)

    newCache = 
        if isReadHit
            then cache
            -- TODO: Populate the cache block content with consecutive memory addresses
            else Cache (cacheSize cache) (associativity cache) (blockSize cache) (Map.insert (cacheIndex, cacheTag) (CacheBlock True False []) (cacheContent cache))

{-  Attempts to do a cache write of a memory address to the specified cache.
    Returns the new cache containing the memory address and whether the cache write was a hit.
-}
cacheWrite :: Cache -> MemoryAddress -> (Cache, IsWriteHit)
cacheWrite cache memoryAddress = (cache, Map.member (cacheIndex, cacheTag) (cacheContent cache)) where
    numCacheBlocks = (cacheSize cache) `div` (blockSize cache)
    cacheIndex = memoryAddress `mod` numCacheBlocks
    cacheTag = memoryAddress `div` numCacheBlocks

{-  Invalidates a memory address on the specified cache.
    Returns the new cache with the specified memory address invalidated.
-}
cacheInvalidate :: Cache -> MemoryAddress -> Cache
cacheInvalidate cache memoryAddress = newCache where
    numCacheBlocks = (cacheSize cache) `div` (blockSize cache)
    cacheIndex = memoryAddress `mod` numCacheBlocks
    cacheTag = memoryAddress `div` numCacheBlocks

    newCache = Cache (cacheSize cache) (associativity cache) (blockSize cache) (Map.adjust blockInvalidate (cacheIndex, cacheTag) (cacheContent cache))

{-  Invalidates a cache block by setting the valid bit to 0 (False).
    Returns the invalidated cache block.
-}
blockInvalidate :: CacheBlock -> CacheBlock
blockInvalidate cacheBlock = CacheBlock False (isBlockDirty cacheBlock) (blockContent cacheBlock)