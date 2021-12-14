{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module D14.Solution (solve) where

import Control.Applicative (liftA2)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (Sum), getSum)
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d14-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input
  print $ solve2 input

type Input = (String, Rules)

type Pair = (Char, Char)

type Rules = M.Map Pair Char

type Counter a = M.Map a (Sum Int)

type CounterKV a = (a, Sum Int)

--- part 1

zipPairs :: [a] -> [(a, a)]
zipPairs xs = zip xs (tail xs)

pairMap :: Rules -> M.Map Pair [Pair]
pairMap = M.mapWithKey (\(x, y) z -> [(x, z), (z, y)])

-- We could formulate this as a more complicated update instead of recreating
-- the map, but the number of keys is not that large.
evolveCount :: Ord a => M.Map a [a] -> Counter a -> Counter a
evolveCount rules = M.fromListWith (<>) . concatMap replaceKV . M.toList
  where
    replaceKV (k, v) = maybe [(k, v)] (fmap (,v)) $ M.lookup k rules

-- Each character is double-counted, except for the two ends which appear once
-- only. Sneaky little trick: since they are the only two odd numbers, we just
-- need to round up when we divide by 2.
countsFromPairs :: Counter Pair -> Counter Char
countsFromPairs = (fmap . fmap) ((`div` 2) . (+ 1)) . M.fromListWith (<>) . produce
  where
    produce :: Counter Pair -> [CounterKV Char]
    produce = concatMap (\((x, y), v) -> [(x, v), (y, v)]) . M.toList

countList :: Ord a => [a] -> Counter a
countList = M.fromListWith (<>) . fmap (,Sum 1)

solveN :: Int -> Input -> Int
solveN generations (str, rules) = do
  let initPairCounts = countList . zipPairs $ str
  let evolve = evolveCount . pairMap $ rules
  let finalPairCounts = iterate evolve initPairCounts !! generations
  let finalCounts = countsFromPairs finalPairCounts
  getSum $ maximum finalCounts - minimum finalCounts

solve1 :: Input -> Int
solve1 = solveN 10

--- part 2

solve2 :: Input -> Int
solve2 = solveN 40

--- parsing

inputP :: Parser Input
inputP = do
  chars <- many1 letter
  _ <- many1 endOfLine
  rules <- M.fromList <$> sepEndBy ruleP endOfLine
  return (chars, rules)
  where
    ruleP :: Parser (Pair, Char)
    ruleP = liftA2 (,) (liftA2 (,) letter letter) (string " -> " >> letter)

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
