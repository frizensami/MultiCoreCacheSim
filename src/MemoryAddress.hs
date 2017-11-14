module MemoryAddress (parse) where 

import Data.Bits as Bits
import Data.Int
import Definitions

main = print $ parse 8 2 36

parse :: BlockSize -> NumCacheSets -> MemoryAddress -> (Tag, SetIndex, Offset)
parse blockSize numCacheSets memoryAddress = (tag, setIndex, offset) where 
    numOffsetBits = (floor . logBase 2.0 . fromIntegral) blockSize
    numSetIndexBits = (floor . logBase 2.0 . fromIntegral) numCacheSets
    numTagBits = 32 - numOffsetBits - numSetIndexBits

    offsetBitmask = blockSize - 1
    setIndexBitmask = Bits.shiftL (numCacheSets - 1) numOffsetBits
    tagBitmask = Bits.complement $ fromIntegral (offsetBitmask + setIndexBitmask) :: Int32

    offset = fromIntegral $ fromIntegral (blockSize - 1) .&. memoryAddress
    setIndex = fromIntegral $ fromIntegral (numCacheSets - 1) .&. shiftR memoryAddress numOffsetBits
    tag = fromIntegral $ (2 ^ numTagBits - 1) .&. shiftR memoryAddress (numOffsetBits + numSetIndexBits)
