{-# LANGUAGE TupleSections #-}

module D11.Solution (solve) where

import Control.Monad ((>=>))
import Data.Array
import Data.Foldable (find, toList)
import Data.Function (on)
import Data.List (group, groupBy, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Monoid (Sum (Sum, getSum))
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe Control.Monad

-- $> testInput = unsafePerformIO $ readFile "inputs/d11-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input
  print $ solve2 input

type Idx = (Int, Int)

type Bounds = (Idx, Idx)

neighborhood :: Idx -> [Idx]
neighborhood (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

neighborhoodInBounds :: Bounds -> Idx -> [Idx]
neighborhoodInBounds b = filter (inRange b) . neighborhood

type Field = Array Idx Int

-- for debug; will break if cells have string length longer than 1 :)
printField :: Field -> String
printField =
  unlines
    . fmap (concatMap snd)
    . groupBy ((==) `on` (fst . fst))
    . assocs
    . fmap show

-- Flashing step: iterative.
--
-- We start by collecting flash updates. A squid above 9 yields nine updates:
-- one for itself plus one each of its 8 neighbors. We count updates for each
-- flashed squid. The output is suited to Data.Array's @accum@.
collecFlashUpdates :: Field -> [(Idx, Int)]
collecFlashUpdates arr =
  countGroups
    . foldMap (neighborhoodInBounds (bounds arr) . fst)
    . filter ((> 9) . snd)
    . assocs
    $ arr
  where
    countGroups :: Ord a => [a] -> [(a, Int)]
    countGroups = fmap (\x -> (head x, length x)) . group . sort

-- We then apply our updates. A flashed squid is disabled (set to 0) if it
-- emitted a flash, and remains disabled even if it was flashed.
-- disable > 9 cells by setting them to 0.
--
-- cell value -> number of flashes -> final value
flash :: Int -> Int -> Int
flash x nf
  | x <= 0 = 0 -- has flashed at this step, will not power up
  | x > 9 = 0 -- has just flashed
  | otherwise = x + nf -- does not matter if we go beyond 9

-- Since one flash may trigger more flasehs, this is where the step needs
-- iteration. We count how many updates happened at any given step, to help
-- stopping the iteration. Alternatively, we could compare the fields by
-- equality to find when it stabilizes.
flashStepIteration :: Field -> (Int, Field)
flashStepIteration arr =
  let updates = collecFlashUpdates arr
   in (length updates, accum flash arr updates)

--- $> let f = fmap (+2) testInput in putStr . printField $ flashStep f

-- iterates flashing until stable
flashStep :: Field -> Field
flashStep =
  snd
    . until ((== 0) . fst) (flashStepIteration . snd)
    . flashStepIteration

-- performs one full step, tracking the number of flashes in the tuple monad
step :: Field -> (Sum Int, Field)
step = countUpdates . flashStep . fmap (+ 1)
  where
    countUpdates :: Field -> (Sum Int, Field)
    countUpdates arr = (foldMap (\x -> if x == 0 then Sum 1 else mempty) arr, arr)

-- composes the same Kleisli arrow n times
kleisliReplicate :: Monad m => Int -> (a -> m a) -> (a -> m a)
kleisliReplicate n = foldr (>=>) return . replicate n

stepN :: Int -> Field -> (Sum Int, Field)
stepN n = kleisliReplicate n step

-- $> putStr . printField . snd . stepN 3 $ testInput

solve1 :: Field -> Int
solve1 = getSum . fst . stepN 100

solve2 :: Field -> Int
solve2 = length . takeWhile (not . allFlashed) . iterate (snd . step)
  where
    allFlashed = all (== 0)

--- parsing

inputP :: Parser Field
inputP = toArray <$> sepEndBy (many1 intDigitP) endOfLine
  where
    intDigitP :: Parser Int
    intDigitP = read . (: []) <$> digit
    toArray :: [[Int]] -> Field
    toArray xs = listArray ((0, 0), (nx -1, ny -1)) (concat xs)
      where
        ny = length xs
        -- unsafe
        nx = length (head xs)

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
