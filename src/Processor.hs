-- |This module defines methods for interacting with and creating processors
module Processor 
    ( createProcessor
    , runOneCycle
    ) where

import Definitions
import Trace
import Bus

type HasConsumedTrace = Bool

createProcessor :: ProtocolInput ->  CacheSize -> Associativity -> BlockSize -> Processor
createProcessor protocolInput cacheSize associativity blockSize = 1

runOneCycle :: Processor -> Maybe Trace -> CacheEventBus -> (Processor, HasConsumedTrace, CacheEventBus)
runOneCycle processor trace eventBus = error "TBI"

-- |Processor runs through all bus events in order and consumes any messages for them, and returns any new messages onto the message list
runMessages :: Processor -> [Message] -> (Processor, [Message])
runMessages processor trace = error "TBI"