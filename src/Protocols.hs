module Protocols where

import MESIProtocol
import DragonProtocol

data ProtocolStates = MESIProtocol MESIState | DragonProtocol DragonState deriving (Show)
