module Protocols where

import MESIProtocol

-- | STUB TYPE:
type DragonState = String

data ProtocolStates = MESIProtocol MESIState | DragonProtocol DragonState deriving (Show)
