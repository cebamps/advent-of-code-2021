{-# LANGUAGE ApplicativeDo #-}

module Solution.D05 (solve) where

import Control.Applicative
import Data.List (group, sort)
import Text.Parsec hiding (Line)
import Text.Parsec.String

--import Input.D05

type Coord = (Int, Int)

type Line = (Coord, Coord)

isAligned :: Line -> Bool
isAligned ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

-- coordinates covered by an axis-aligned line
coveredAligned :: Line -> [Coord]
coveredAligned line = case line of
  ((x1, y), (x2, y2)) | y2 == y -> [(x, y) | x <- range x1 x2]
  ((x, y1), (x2, y2)) | x2 == x -> [(x, y) | y <- range y1 y2]
  ((x1, y1), (x2, y2)) | x1 - x2 == y1 - y2 -> [(x1 + d, y1 + d) | d <- range 0 (x2 - x1)]
  ((x1, y1), (x2, y2)) | x1 - x2 == y2 - y1 -> [(x1 + d, y1 - d) | d <- range 0 (x2 - x1)]
  _ -> error $ "bad line: " ++ show line
  where
    range x1 x2 = if x1 < x2 then [x1 .. x2] else [x2 .. x1]

-- $> coveredAligned ((0,0),(1,1))

covered :: [Line] -> [Int]
covered = fmap length . group . sort . concatMap coveredAligned

answer1 :: [Line] -> Int
answer1 = answer2 . filter isAligned

answer2 :: [Line] -> Int
answer2 = length . filter (>= 2) . covered

solve :: String -> IO ()
solve input_ = do
  input <- parseOrFail parseLines input_
  print $ answer1 input
  print $ answer2 input

--- $> readFile "inputs/d05.txt" >>= solve

-- parsing

parsePair :: Parser (Int, Int)
parsePair = do
  d1 <- many1 digit
  _ <- char ','
  d2 <- many1 digit
  return (read d1, read d2)

parseLine :: Parser Line
parseLine = do
  l <- parsePair
  _ <- string " -> "
  r <- parsePair
  return (l, r)

parseLines :: Parser [Line]
parseLines = sepEndBy parseLine endOfLine

parseOrFail :: Parsec String () a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""

--- $> parseFromFile parseLines "inputs/d05-test.txt"
