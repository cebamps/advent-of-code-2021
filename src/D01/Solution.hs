module D01.Solution (main) where

import Data.List (tails)
import D01.Input (input)

zipPairs :: [a] -> [(a, a)]
zipPairs = zip <*> tail

countIncreases :: Ord a => [a] -> Int
countIncreases = length . filter (uncurry (<)) . zipPairs

windows :: Num a => Int -> [a] -> [[a]]
windows len = filter ((== len) . length) . fmap (take len) . tails

summedWindows :: Num a => Int -> [a] -> [a]
summedWindows len = fmap sum . windows len

main :: IO ()
main = do
  print $ countIncreases input
  print $ countIncreases . summedWindows 3 $ input
