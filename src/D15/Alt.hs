{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module D15.Alt (solve) where

import D23.Graph
import Data.Array.IArray
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d15-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input
  print $ solve2 input

type Idx = (Int, Int)

type Field a = Array Idx a

type Risk = Field Int

--- part 1

fin :: Field a -> Idx
fin = snd . bounds

neighbors :: Idx -> [Idx]
neighbors (y, x) = [(y, x -1), (y + 1, x), (y, x + 1), (y -1, x)]

solve1 :: Risk -> Int
solve1 risk = do
  let adj :: Idx -> [(Int, Idx)]
      adj i = (\i' -> (risk ! i', i')) <$> filter (inRange (bounds risk)) (neighbors i)
  let p = SparseDijkstra adj
  dijkstra p (0, 0) (fin risk)

--- part 2

tileRisk :: Risk -> Risk
tileRisk risk = array newBounds (concatMap newAssocs . assocs $ risk)
  where
    (ylen, xlen) = let ((0, 0), (y, x)) = bounds risk in (y + 1, x + 1)
    newBounds :: (Idx, Idx)
    newBounds = ((0, 0), (5 * ylen - 1, 5 * xlen - 1))
    newAssocs :: (Idx, Int) -> [(Idx, Int)]
    newAssocs ((y, x), v) =
      [ ((y + ylen * ty, x + xlen * tx), v')
        | d <- [0 .. 24],
          -- tiled y and x
          let (ty, tx) = d `divMod` 5
              v' = wrap $ v + tx + ty
      ]
    wrap :: Int -> Int
    wrap x = (x - 1) `mod` 9 + 1

solve2 :: Risk -> Int
solve2 = solve1 . tileRisk

--- parsing

inputP :: Parser Risk
inputP = do
  rows@(row1 : _) <- sepEndBy (many1 (read . (: []) <$> digit)) endOfLine
  return $ listArray ((0, 0), (length rows - 1, length row1 - 1)) $ concat rows

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
