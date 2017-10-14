-- |This module defines methods for interacting with and creating processors
module Processor 
    ( createProcessor
    ) where

import Definitions

createProcessor :: ProtocolInput ->  CacheSize -> Associativity -> BlockSize -> Processor
createProcessor protocolInput cacheSize associativity blockSize = 1