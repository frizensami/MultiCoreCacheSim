module Trace
    ( toTraceWithError
    , Trace(..)
    ) where

import Definitions


-- |Define the structure of the main source of input into the program: a single trace line. Can either be a load, store or other instructions where we wait for a certain number of cycles.
data Trace = LoadInstruction Address | StoreInstruction Address | OtherInstruction NumCycles deriving (Show)

-- |This function converts a String to a valid trace. It can cause an error if it is called with an input that is not of the right format.
toTraceWithError :: String -> Trace
toTraceWithError traceString = 
    let traceComponents = words traceString
        firstWord = traceComponents!!0
        secondInt = read $ traceComponents!!1 :: Int
    in
    case firstWord of
        "0" -> LoadInstruction  secondInt
        "1" -> StoreInstruction secondInt
        "2" -> OtherInstruction secondInt
        _ -> error "FATAL ERROR: Cannot parse this trace string!"




