module Protocols where

import MESIProtocol
import DragonProtocol
import IllinoisProtocol

data ProtocolStates = MESIProtocol MESIState | DragonProtocol DragonState | IllinoisProtocol IllinoisState deriving (Show)
