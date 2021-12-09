module D09.Solution (solve) where

import Control.Comonad
import Control.Comonad.Store
import Control.Monad ((>=>))
import Data.List (group, sort)
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Monoid (Sum (Sum, getSum))
import Text.Parsec
import Text.Parsec.String (Parser)

-- might as well leave my ghcid debug prints
--
-- $> :m + System.IO.Unsafe Data.List
--
--- $> input = unsafePerformIO $ readFile "inputs/d09.txt" >>= parseOrFail parseInput
--
-- $> testInput = unsafePerformIO $ readFile "inputs/d09-test.txt" >>= parseOrFail parseInput
--
-- $> mapM_ print testInput

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail parseInput inputStr
  print $ solve1 input
  print $ solve2 input

type Idx = (Int, Int)

type Grid a = [[a]]

type Field a = Store Idx (Maybe a)

-- my first Kleisli arrow composition
gridGet :: Idx -> Grid a -> Maybe a
gridGet (x, y) = safeIndex y >=> safeIndex x
  where
    safeIndex :: Int -> [a] -> Maybe a
    safeIndex n
      | n >= 0 = listToMaybe . drop n
      | otherwise = const Nothing

gridGetDefault :: a -> Idx -> Grid a -> a
gridGetDefault empty idx = fromMaybe empty . gridGet idx

gridIndexes :: Grid a -> [Idx]
gridIndexes gr = do
  (y, row) <- indexed gr
  (x, _) <- indexed row
  return (x, y)
  where
    indexed :: [a] -> [(Int, a)]
    indexed = zip [0 ..]

-- my first use of Comonad
--
-- It's going to be a bit clumsy. For example, we lose any notion of
-- enumeration and bounds because we construct the store from an accessor
-- function. There's probably a better way with a custom comonad instance based
-- on a map with default values, but I don't have time to figure that out.
gridField :: Grid a -> Field a
gridField gr = store (`gridGet` gr) (0, 0)

idxShift :: Idx -> Idx -> Idx
idxShift (dx, dy) (x, y) = (x + dx, y + dy)

neighborIndexes :: [Idx]
neighborIndexes = [(-1, 0), (1, 0), (0, -1), (0, 1)]

adjacents :: Field Int -> [Int]
adjacents f = mapMaybe ((`peeks` f) . idxShift) neighborIndexes

riskFactor :: Field Int -> Maybe Int
riskFactor f = do
  let neighbors = adjacents f
  v <- extract f

  if all (> v) neighbors
    then return $ v + 1
    else Nothing

foldField :: Monoid m => (Field a -> m) -> Grid a -> m
foldField f gr = do
  let ids = gridIndexes gr
  let fieldQ = fst . runStore $ extend f $ gridField gr
  foldMap fieldQ ids

maybeEmpty :: Monoid m => Maybe m -> m
maybeEmpty Nothing = mempty
maybeEmpty (Just a) = a


solve1 :: Grid Int -> Int
solve1 = getSum . maybeEmpty . foldField (fmap Sum <$> riskFactor)

--- part 2
--
-- Oh no. Now I'm invested with Comonad, so I'll go for the obvious solution I
-- can see, which is going to be very inefficient!
--
-- Given a focus point, I will walk down any available slope. The lowest point
-- I ever walk to will be the local minimum for that focus point, and will
-- uniquely identify the basin.
--
-- Uniquely because the exercise says so. Indeed otherwise we could have
-- something like this with more than one low point. But then, 2 doesn't flow
-- down into 1 and 3 flows down into both 1s:
--
-- 999999
-- 913219
-- 999999

bottomIndex :: Field Int -> Maybe Idx
bottomIndex = listToMaybe . fmap pos . focusLower

focusLower :: Field Int -> [Field Int]
-- special cases
focusLower f
  | isNothing (extract f) = []
  | extract f == Just 9 = []
focusLower f = do
  -- bit of a hack to safely get to the Int value
  value <- maybeToList $ extract f
  let neighbors = (`seeks` f) . idxShift <$> neighborIndexes
  let lowerNeighbors = filter (maybe False (< value) . extract) neighbors

  case lowerNeighbors of
    [] -> return f -- local minimum
    _ -> lowerNeighbors >>= focusLower -- recurse

basinBottoms :: Grid Int -> [Idx]
basinBottoms = catMaybes . foldField ((: []) . bottomIndex)

-- reverse . sort is not cool for large lists, but it's fine here
descendingGroupSizes :: Ord a => [a] -> [Int]
descendingGroupSizes = reverse . sort . fmap length . group . sort

-- $> bottomIndex $ seek (0,3) $ gridField testInput

solve2 :: Grid Int -> Int
solve2 = product . take 3 . descendingGroupSizes . basinBottoms

--- parsing

parseInput :: Parser (Grid Int)
parseInput = sepEndBy (many1 parsedDigit) endOfLine
  where
    parsedDigit :: Parser Int
    parsedDigit = read . (: []) <$> digit

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
