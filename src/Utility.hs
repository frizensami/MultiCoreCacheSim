module Utility where

removeNthElement :: [a] -> Int -> [a]
removeNthElement xs n = let (ys,zs) = splitAt n xs in ys ++ (tail zs)