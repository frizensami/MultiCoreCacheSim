-- |This module defines methods for interacting with and creating processors
module Processor 
    ( createProcessor
    , runOneCycle
    ) where

import Definitions
import Trace
import Bus
import qualified Debug.Trace as T

type HasConsumedTrace = Bool

createProcessor :: ProtocolInput ->  CacheSize -> Associativity -> BlockSize -> Processor
createProcessor protocolInput cacheSize associativity blockSize = 1

-- TO BE IMPLEMENTED - THIS IS FOR TESTING FLOW
runOneCycle :: Processor -> Maybe Trace -> CacheEventBus -> (Processor, HasConsumedTrace, CacheEventBus)
runOneCycle processor (Just trace) eventBus = T.trace ((show processor) ++ ": " ++ (show trace)) $ (processor, True, eventBus) 
runOneCycle processor Nothing eventBus      = (processor, True, eventBus) 

-- |Processor runs through all bus events in order and consumes any messages for them, and returns any new messages onto the message list
runMessages :: Processor -> [Message] -> (Processor, [Message])
runMessages processor trace = error "TBI"