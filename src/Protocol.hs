module Protocol (read, write) where

import Definitions
import CacheSet
import Data.Array as Array
import qualified DragonCacheModifier
import qualified MESICacheModifier
import Prelude hiding (read, write)

read :: Protocol -> (BlockTag, SetIndex, Offset) -> Array Int CacheSet -> (IsBusy, IsReadHit, Array Int CacheSet)
read MESI (blockTag, setIndex, offset) cacheStructure = MESICacheModifier.read (blockTag, setIndex, offset) cacheStructure
read Dragon (blockTag, setIndex, offset) cacheStructure = DragonCacheModifier.read (blockTag, setIndex, offset) cacheStructure

write :: Protocol -> (BlockTag, SetIndex, Offset) -> Array Int CacheSet -> (IsBusy, IsWriteHit, Array Int CacheSet)
write MESI (blockTag, setIndex, offset) cacheStructure = MESICacheModifier.write (blockTag, setIndex, offset) cacheStructure
write Dragon (blockTag, setIndex, offset) cacheStructure = DragonCacheModifier.write (blockTag, setIndex, offset) cacheStructure
