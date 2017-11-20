module Protocol (ProtocolState (..)) where

class ProtocolState a where
    isDone :: a -> Bool
