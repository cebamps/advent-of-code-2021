{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module D15.Solution (solve) where

import Data.Array.IArray
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Set (member)
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d15-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  --print $ solve1 input
  print $ solve2 input

type Idx = (Int, Int)

type Field a = Array Idx a

type Risk = Field Int

--- part 1

-- okay, this is going to use Dijkstra's algorithm, but how do I implement this
-- in Haskell?? haha.

type Distances = Field (Maybe Int)

type Visited = S.Set Idx

data SearchState = SearchState
  { currentEdge :: Idx,
    unvisited :: Visited,
    distances :: Distances
  }
  deriving (Show)

idxMin :: Ord a => [(b, a)] -> Maybe b
idxMin [] = Nothing
idxMin xs = Just . fst . minimumBy (comparing snd) $ xs

expand :: Risk -> SearchState -> Maybe SearchState
expand risk search = do
  let distances' = updateDistances risk search (currentEdge search)
  let unvisited' = currentEdge search `S.delete` unvisited search
  currentEdge' <- idxMin . mapMaybe (\x -> sequenceA (x, distances' ! x)) . S.elems $ unvisited'

  return SearchState {unvisited = unvisited', distances = distances', currentEdge = currentEdge'}

updateDistances :: Risk -> SearchState -> Idx -> Distances
updateDistances risk search idx = accum updateElem (distances search) (distanceUpdate <$> nidx)
  where
    keepIdx x = inRange (bounds risk) x && x `member` unvisited search
    nidx = filter keepIdx $ neighbors idx
    distanceUpdate :: Idx -> (Idx, Maybe Int)
    distanceUpdate x = (x,) $ (+ risk ! x) <$> (distances search ! idx)
    updateElem :: Maybe Int -> Maybe Int -> Maybe Int
    updateElem Nothing x = x
    updateElem x Nothing = x -- won't happen
    updateElem (Just x) (Just x') = Just $ min x x'

-- indexing the array forces computation
expandUntilDone :: Risk -> SearchState -> SearchState
expandUntilDone risk search = case expand risk search of
  Nothing -> search
  Just search' -> expandUntilDone risk search'

fin :: Field a -> Idx
fin = snd . bounds

neighbors :: Idx -> [Idx]
neighbors (y, x) = [(y, x -1), (y + 1, x), (y, x + 1), (y -1, x)]

initialState :: Risk -> SearchState
initialState risk = SearchState start (S.fromList . range . bounds $ risk) ((Nothing <$ risk) // [(start, Just 0)])
  where
    start = (0, 0)

solve1 :: Risk -> Maybe Int
solve1 risk = distances finalState ! fin risk
  where
    finalState = expandUntilDone risk (initialState risk)

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

solve2 :: Risk -> Maybe Int
solve2 = solve1 . tileRisk

-- $> tileRisk testInput ! (49,49)

-- $> solve2 testInput

--- parsing

inputP :: Parser Risk
inputP = do
  rows@(row1 : _) <- sepEndBy (many1 (read . (: []) <$> digit)) endOfLine
  return $ listArray ((0, 0), (length rows - 1, length row1 - 1)) $ concat rows

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
