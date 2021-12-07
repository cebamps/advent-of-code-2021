module D07.Solution (solve) where

import Data.Functor (($>))
import Data.List (minimumBy)
import Data.Monoid (Sum (..))
import Data.Ord (comparing)
import Text.Parsec
import Text.Parsec.String (Parser)

solve :: String -> IO ()
solve inputStr = do
  let input = parseCrabPositions inputStr
  print $ solve1 <$> input
  print $ solve2 <$> input

--- solution

-- The solution is somewhere inbetween the most extreme crab positions. Doing
-- one sweep is easiest.

-- This is the result of refactoring to pass the fuel cost computation after
-- seeing part 2.

-- distance -> fuel cost
type CostFunction = Int -> Int

-- initial -> final -> total fuel cost
type TotalCostFunction = CrabPositions -> CrabPositions -> Int

concatZipWith :: (Monoid m) => (a -> b -> m) -> [a] -> [b] -> m
concatZipWith f xs ys = mconcat $ zipWith f xs ys

mkTotalCost :: CostFunction -> TotalCostFunction
mkTotalCost fuelForDist c1 c2 = getSum $ concatZipWith (\x y -> (Sum . fuelForDist . abs) (x - y)) c1 c2

sweepFinalCrabPositions :: CrabPositions -> [CrabPositions]
sweepFinalCrabPositions initPos = [initPos $> coord | coord <- [minPos .. maxPos]]
  where
    minPos = minimum initPos; maxPos = maximum initPos

mostEfficientFinalCrabPositions :: TotalCostFunction -> CrabPositions -> CrabPositions
mostEfficientFinalCrabPositions costFun initPos = minimumBy (comparing $ costFun initPos) (sweepFinalCrabPositions initPos)

solveForCostFunction :: TotalCostFunction -> CrabPositions -> Int
solveForCostFunction costFun initPos = costFun (mostEfficientFinalCrabPositions costFun initPos) initPos

solve1 :: CrabPositions -> Int
solve1 = solveForCostFunction $ mkTotalCost id

solve2 :: CrabPositions -> Int
solve2 = solveForCostFunction $ mkTotalCost arith
  where
    arith x = (x * (x + 1)) `div` 2

--- parsing

type CrabPositions = [Int]

crabPositionParser :: Parser CrabPositions
crabPositionParser = fmap read <$> sepBy (many1 digit) (char ',')

parseCrabPositions :: String -> Maybe CrabPositions
parseCrabPositions = either (const Nothing) Just . parse crabPositionParser ""
