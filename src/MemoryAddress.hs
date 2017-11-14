module MemoryAddress (parse) where 

import CacheParams
import Data.Bits as Bits
import Data.Int
import Definitions

main = print $ parse (createCacheParams 32 2 8) 36

-- |Parses a memory address into the respective tag, set index, and offset while adhering to the 
--  specified block size and number of cache sets.
--  Returns the tag, set index, and offset in a tuple.
parse :: CacheParams -> MemoryAddress -> (Tag, SetIndex, Offset)
parse cacheParams memoryAddress = (tag, setIndex, offset) where 
    numOffsetBits = (floor . logBase 2.0 . fromIntegral) (blockSize cacheParams)
    numSetIndexBits = (floor . logBase 2.0 . fromIntegral) (numCacheSets cacheParams)
    numTagBits = 32 - numOffsetBits - numSetIndexBits

    offsetBitmask = (blockSize cacheParams) - 1
    setIndexBitmask = Bits.shiftL ((numCacheSets cacheParams) - 1) numOffsetBits
    tagBitmask = Bits.complement $ fromIntegral (offsetBitmask + setIndexBitmask) :: Int32

    offset = fromIntegral $ fromIntegral ((blockSize cacheParams) - 1) .&. memoryAddress
    setIndex = fromIntegral $ fromIntegral ((numCacheSets cacheParams) - 1) .&. shiftR memoryAddress numOffsetBits
    tag = fromIntegral $ (2 ^ numTagBits - 1) .&. shiftR memoryAddress (numOffsetBits + numSetIndexBits)
