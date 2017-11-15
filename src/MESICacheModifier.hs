module MESICacheModifier where

import Data.Array as Array
import Definitions
import Prelude hiding (read, write)

read :: (BlockTag, SetIndex, Offset) -> Array Int CacheSet -> (IsBusy, IsReadHit, Array Int CacheSet)
read (blockTag, setIndex, offset) cacheStructure = (isBusy, isReadHit, newCacheStructure) where 
    isBusy = False
    isReadHit = False
    newCacheStructure = cacheStructure

write :: (BlockTag, SetIndex, Offset) -> Array Int CacheSet -> (IsBusy, IsWriteHit, Array Int CacheSet)
write (blockTag, setIndex, offset) cacheStructure = (isBusy, isWriteHit, newCacheStructure) where 
    isBusy = False
    isWriteHit = False
    newCacheStructure = cacheStructure
