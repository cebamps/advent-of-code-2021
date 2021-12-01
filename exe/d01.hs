module Main where

import Data.List (tails)
import Input.D01 (input)

zipPairs :: [a] -> [(a, a)]
zipPairs = zip <*> tail

countIncreases :: Ord a => [a] -> Int
countIncreases depths = length increases
  where
    increases = [() | (x, y) <- zipPairs depths, x < y]

windows :: Num a => Int -> [a] -> [[a]]
windows len = filter ((== len) . length) . fmap (take len) . tails

summedWindows :: Num a => Int -> [a] -> [a]
summedWindows len = fmap sum . windows len

main :: IO ()
main = do
  print $ countIncreases input
  print $ countIncreases . summedWindows 3 $ input
