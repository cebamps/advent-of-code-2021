{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module D17.Solution (solve) where

import Data.List (findIndices, nub, partition, unfoldr)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d17-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input
  print $ solve2 input
  debug input

-- xmin, xmax, ymin, ymax
type Area = (Int, Int, Int, Int)

type Pos = (Int, Int)

type Vel = (Int, Int)

inArea :: Area -> Pos -> Bool
inArea a (x, y) = inAreaX a x && inAreaY a y

inAreaX :: Area -> Int -> Bool
inAreaX (xmin, xmax, _, _) x = xmin <= x && x <= xmax

inAreaY :: Area -> Int -> Bool
inAreaY (_, _, ymin, ymax) y = ymin <= y && y <= ymax

-- assumption: the trench is ahead of and below us
beyondAreaX :: Area -> Int -> Bool
beyondAreaX (_, xmax, _, _) x = x > xmax

beyondAreaY :: Area -> Int -> Bool
beyondAreaY (_, _, ymin, _) y = y < ymin

beyondArea :: Area -> Pos -> Bool
beyondArea a (x, y) = beyondAreaX a x || beyondAreaY a y

--- part 1

-- First, observe that although x and y movement are independent, the total
-- trajectory time is constrained by the problem in x. If there is an x
-- velocity such that x velocity stops in the target, then we can take all the
-- time we need to travel upwards beyond the time it takes to stop on the x
-- axis. There are a few extra times to check before that, as well.

-- Also observe that for positive initial y velocity v0y, at steps t = v0y
-- (exit) velocity is zero -- we have reached the apex -- and at step t = 2*v0y
-- + 1 we are back where we started, with (exit) negative velocity (-v0y - 1).
-- The next step will therefore be at (-v0y - 1).

-- If we assume that x can stop inside of our area, the maximum y velocity is
-- easy to find. It is the one that will make us reach the bottom of the trench
-- at time (2 * v0y + 2), i.e., such that (-v0y - 1) = y_bottom. Anything above
-- that will make it miss the shot immediately.

-- Might as well do everything with pen and paper :)
solve1 :: Area -> Int
solve1 (_, _, ymin, _) = let v0y = - ymin - 1 in (v0y * (v0y + 1)) `div` 2

--- part 2

-- This is a bit trickier. The search space is big, but we can divide and
-- conquer.
--
-- We'll solve for y first, accumulating (t, v0y) pairs representing trajectory
-- length and initial velocity when inside the area. We can condense that into
-- a map from time to velocities.
--
-- Then we'll solve for x the same way, except that now we will likely find
-- infinite (t, v0x) solutions for some v0x. Time is upper-bounded on y though,
-- so we will know when to stop looking for v0y values that reach the area at
-- the same time on the y axis.

evolveY :: Int -> [Int]
evolveY v0 = unfoldr (\(y, v) -> Just (y, (y + v, v - 1))) (0, v0)

-- assumes nonnegative velocity, does not terminate
evolveX :: Int -> [Int]
evolveX = repeatLast . evolveX'

-- errors on empty list
repeatLast :: [a] -> [a]
repeatLast xs = xs ++ repeat (last xs)

-- terminating version. I had a bug here that got me the right solution on part
-- 1 and the wrong one on part 2, because I forgot the last step :(
evolveX' :: Int -> [Int]
evolveX' v0 = unfoldr (\(x, v) -> if v < 0 then Nothing else Just (x, (x + v, v - 1))) (0, v0)

-- always terminates
makeSequenceY :: Area -> Int -> [Int]
makeSequenceY area = takeWhile (not . beyondAreaY area) . evolveY

-- For this one we have to handle three cases: we stop before the area, we stop
-- inside the area, or we reach beyond it. We only return an infinite list if
-- the last value is inside the area to avoid risking non-termination, but we
-- will not use this function on low values of v0x anyway.
makeSequenceX :: Area -> Int -> [Int]
makeSequenceX area v0x = case break (beyondAreaX area) $ evolveX' v0x of
  -- we stopped before reaching beyond the area
  (before, []) | lx <- last before, inAreaX area lx -> before ++ repeat lx
  (_, []) -> [] -- we don't reach the area
  (before, _) -> before

type Solutions1D = S.Set Int

type SolutionMap1D = M.Map Int Solutions1D

search :: (Int -> Bool) -> (Int -> [Int]) -> Int -> [Int]
search isHit makeSequence v0 = findIndices isHit (makeSequence v0)

searchY :: Area -> Int -> [Int]
searchY area = search (inAreaY area) (makeSequenceY area)

searchX :: Area -> Int -> [Int]
searchX area = search (inAreaX area) (makeSequenceX area)

-- run a 1D solution search for a range of initial velocities
solutionMap :: (Int -> [Int]) -> [Int] -> SolutionMap1D
solutionMap searchFun = M.fromListWith S.union . concatMap produceSolutions
  where
    produceSolutions :: Int -> [(Int, Solutions1D)]
    produceSolutions v0 = (,S.singleton v0) <$> searchFun v0

-- taking a solution map for the y axis, run a solution search on the x axis as
-- with solutionMap
querySolutionMap :: SolutionMap1D -> (Int -> [Int]) -> [Int] -> [Vel]
querySolutionMap solutions searchFun = concatMap collectVelocities
  where
    maxTime :: Int
    maxTime = maybe 0 fst $ M.lookupMax solutions
    collectVelocities :: Int -> [(Int, Int)]
    collectVelocities v0x = do
      -- note: wasted some time here. I used filter instead of takeWhile, but
      -- the list is infinite so it never terminated
      time <- takeWhile (<= maxTime) $ searchFun v0x
      v0y <- maybe [] S.toList $ M.lookup time solutions
      return (v0x, v0y)

vyRange :: Area -> [Int]
vyRange (_, _, ymin, _) = [vmin .. vmax]
  where
    -- see above
    vmax = - ymin - 1
    vmin = ymin - 1

vxRange :: Area -> [Int]
vxRange (xmin, xmax, _, _) = [ceiling $ vxToStop xmin .. xmax + 1]
  where
    -- velocity in x needed to stop at a given x position -- the proof is left
    -- as an exercise to the reader
    vxToStop :: Int -> Double
    vxToStop n = let x = fromIntegral n in (sqrt (8 * x + 1) - 1) / 2

solve2' :: Area -> [Vel]
solve2' area = do
  let ySolutionMap = solutionMap (searchY area) (vyRange area)
  nub $ querySolutionMap ySolutionMap (searchX area) (vxRange area)

solve2 :: Area -> Int
solve2 = length . solve2'

-- 3170 is too high? let's find out which velocities aren't hitting the target.
-- See notes.

debug :: Area -> IO ()
debug = \area -> do
  let vels = solve2' area
  let (ok, wrong) = partition (hits area) vels

  putStrLn "\nDebug."
  print area

  putStr "wrong solutions:"
  if null wrong
    then putStrLn " none"
    else putStrLn "" >> mapM_ print wrong

  putStr "OK count: "
  print $ length ok
  where
    evolveXY :: Area -> Vel -> [Pos]
    evolveXY area (vx, vy) = takeWhile (not . beyondArea area) $ zip (evolveX vx) (evolveY vy)
    hits :: Area -> Vel -> Bool
    hits area = any (inArea area) . evolveXY area

-- $> solutionMap (searchY testInput) (vyRange testInput)

-- $> debug testInput

--- parsing

inputP :: Parser Area
inputP =
  (,,,)
    <$> (string "target area: x=" *> numP)
    <*> (string ".." *> numP)
    <*> (string ", y=" *> numP)
    <*> (string ".." *> numP)
  where
    numP :: Parser Int
    numP = fmap read $ do
      sign <- optionMaybe $ char '-'
      digits <- many1 digit
      return $ maybe id (:) sign digits

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
