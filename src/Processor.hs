-- |This module defines methods for interacting with and creating processors
module Processor 
    ( createProcessor
    , runOneCycle
    ) where

import Definitions
import Trace

type HasConsumedTrace = Bool

createProcessor :: ProtocolInput ->  CacheSize -> Associativity -> BlockSize -> Processor
createProcessor protocolInput cacheSize associativity blockSize = 1

runOneCycle :: Processor -> Maybe Trace -> (Processor, HasConsumedTrace, [BusEvent])
runOneCycle processor trace = error "TBI"

-- |Processor runs through all bus events in order and returns new state and any additional bus events for the rest of the processors
runBusEvents :: Processor -> [BusEvent] -> (Processor, [BusEvent])
runBusEvents processor trace = error "TBI"