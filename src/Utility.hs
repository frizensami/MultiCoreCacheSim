module Utility where

removeNthElement :: [a] -> Int -> [a]
removeNthElement xs n = let (ys,zs) = splitAt n xs in ys ++ (tail zs)

insertElementAtIdx :: [a] -> Int -> a -> [a]
insertElementAtIdx xs idx x = let (ys,zs) = splitAt idx xs in ys ++ [x] ++ zs