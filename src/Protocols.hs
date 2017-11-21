module Protocols where

import MESIProtocol

-- | STUB TYPE:
type DragonState = String

data ProtocolState = MESIProtocol MESIState | DragonProtocol DragonState deriving (Show)
