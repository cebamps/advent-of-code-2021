module D06.Solution (solve) where

import Control.Applicative
import qualified Data.IntMap.Strict as M
import Data.List (group, iterate', sort)
import Text.Parsec
import Text.Parsec.String (Parser)

solve :: String -> IO ()
solve inputStr = do
  let input = parseInput inputStr
  print $ countFishAfter' 80 <$> input
  print $ countFishAfter' 256 <$> input

---

step :: FishStates -> FishStates
step fish = agedFish ++ newbornFish
  where
    newbornFish = flip replicate 8 . length . filter (== 0) $ fish
    agedFish = fmap age fish
    age 0 = 6
    age x = x - 1

countFishAfter :: Int -> FishStates -> Int
countFishAfter days = length . (!! days) . iterate' step

-- Part 2: We'll need to be more efficient. We now know ordering doesn't
-- matter. Time to refactor!

type FishCounts = M.IntMap Int

countFishInSample :: FishStates -> FishCounts
countFishInSample = M.fromList . fmap (liftA2 (,) head length) . group . sort

stepCounts :: FishCounts -> FishCounts
stepCounts = M.fromListWith (+) . updatePopulation . M.toList
  where
    -- ah, monads, finally
    updatePopulation pops = do
      pop@(age, count) <- pops
      if age == 0
        then -- spawn as many 6-days-olds, regenerate to 8 days
          [(6, count), (8, count)]
        else return (age-1, count)

countFishAfter' :: Int -> FishStates -> Int
countFishAfter' days = sum . (!! days) . iterate' stepCounts . countFishInSample

--- parsing

type FishStates = [Int]

parseInput :: String -> Maybe FishStates
parseInput = either (const Nothing) Just . parse parseFishStates ""
  where
    parseTokens :: Parser [String]
    parseTokens = sepBy (many1 digit) (char ',')
    parseFishStates :: Parser FishStates
    parseFishStates = fmap read <$> parseTokens
