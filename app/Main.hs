module Main where

import Simulator
import System.Environment (getArgs)

-- |The 'main' function reads 5 command line arguments and passes them to the runSimulation function
main :: IO ()
main = do
    args <- getArgs
    if length args < 5 then 
        error "Insufficient number of arguments: need 5" 
    else do
        results <- runSimulation (args!!0) (args!!1) (read $ args!!2) (read $ args!!3) (read $ args!!4) 
        putStrLn results