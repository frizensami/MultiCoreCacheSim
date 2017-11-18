module MemoryAddress (parse) where

import CacheParams (CacheParams)
import qualified CacheParams
import Data.Bits as Bits
import Definitions

main = print $ parse (CacheParams.create 32 2 8) 36

-- |Parses a memory address into the respective block tag, set index, and offset while adhering to the
--  specified block size and number of cache sets.
--  Returns the block tag, set index, and offset in a tuple.
parse :: CacheParams -> MemoryAddress -> (BlockTag, SetIndex, Offset)
parse cacheParams memoryAddress = (blockTag, setIndex, offset) where
    blockSize = CacheParams.getBlockSize cacheParams
    numCacheSets = CacheParams.getNumCacheSets cacheParams

    numOffsetBits = floor $ (logBase :: Float -> Float -> Float) 2.0 $ fromIntegral blockSize
    numSetIndexBits = floor $ (logBase :: Float -> Float -> Float) 2.0 $ fromIntegral numCacheSets
    numTagBits = 32 - numOffsetBits - numSetIndexBits

    offset = fromIntegral $ fromIntegral (blockSize - 1) .&. memoryAddress
    setIndex = fromIntegral $ fromIntegral (numCacheSets - 1) .&. shiftR memoryAddress numOffsetBits
    blockTag = fromIntegral $ (2 ^ numTagBits - 1) .&. shiftR memoryAddress (numOffsetBits + numSetIndexBits)
