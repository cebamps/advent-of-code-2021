module D12.Solution (solve) where

import Control.Applicative (liftA2)
import Control.Monad ((>=>))
import Data.Char (isLower)
import Data.List (group, partition, sort)
import Data.Monoid (Any (..))
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe Data.List

-- $> testInput = unsafePerformIO $ readFile "inputs/d12-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr

  print $ solve1 input
  print $ solve2 input

--- $> testInput

--- part 1

-- Approach: modify the graph as we go, pruning edges leading to the small caves
-- once we have visited it.
--
-- We can probably get stuck by visiting a small cave, as it might cut us off
-- from the exit. Hence we'll need to check connectivity to the exit.
--
-- For example, with a nasty input such as
--
--  end - start - a - B - C
--      \       /
--        - Z -
--
-- we can only go start,end or start,a,Z,end, but a naive approach could lead
-- us down an infinite loop of B <-> C after visiting a.

type Cave = String

type Edge = (Cave, Cave)

-- undirected: both edge directions are represented
type Graph = [(Cave, Cave)]

isSmall :: Cave -> Bool
isSmall = all isLower

buildGraph :: [Edge] -> Graph
buildGraph =
  --  fmap compress
  --    . groupBy ((==) `on` fst)
  --    . sortBy (comparing fst) .
  concatMap bidir
  where
    bidir :: Edge -> [Edge]
    bidir (x, y) = [(x, y), (y, x)]
    compress :: [Edge] -> (Cave, [Cave])
    compress es = ((fst . head) es, fmap snd es)

-- prune a node from the graph (snd), keeping removed edges aside (fst)
pruneGraph :: Cave -> Graph -> ([Edge], Graph)
pruneGraph c = partition $ \(x, y) -> x == c || y == c

neighbors :: Cave -> Graph -> [Cave]
neighbors c gr = [y | (x, y) <- gr, x == c]

connects :: Cave -> Cave -> Graph -> Bool
connects cto cfrom gr = getAny $ go cfrom gr
  where
    go :: Cave -> Graph -> Any
    go c _ | c == cto = Any True
    go c gr =
      let (es, gr') = pruneGraph c gr
       in mconcat $ [go c' gr' | (c', _) <- es, c' /= c]

-- bool: whether we should close the cave
nextStep :: Bool -> Cave -> Graph -> [(Cave, Graph)]
nextStep _ c gr | not (connects "end" c gr) = [] -- give up
nextStep True c gr =
  let (es, gr') = pruneGraph c gr
   in [(c', gr') | c' <- neighbors c es]
nextStep False c gr = [(c', gr) | c' <- neighbors c gr]

-- depth-first
walk :: Cave -> Graph -> [[Cave]]
walk c gr = do
  (c', gr') <- nextStep (isSmall c) c gr
  if c' == "end"
    then return [c']
    else (c' :) <$> walk c' gr'

-- $> walk "start" $ buildGraph [("start","c"), ("c", "end"), ("c","B"),("B","end")]

solve1 :: Graph -> Int
solve1 = length . walk "start"

--- part 2

walk' :: Cave -> Graph -> [[Cave]]
walk' c gr = reverse <$> go [] c gr
  where
    go path c gr = do
      (c', gr') <- nextStep (isSmall c) c gr
      if c' == "end"
        then return $ c' : c : path
        else go (c : path) c' gr'

-- Reconsidering most of the flow, because the graph can't just be pruned
-- incrementally. I move the pruning out instead.
nextStep' :: Cave -> Graph -> [Cave]
nextStep' c gr
  | not (connects "end" c gr) = []
  | otherwise = neighbors c gr

-- Variant of walk for part 2, where we carry the path along. The path ends up
-- reversed and including the starting point.
walk2 :: Cave -> Graph -> [[Cave]]
walk2 c gr = reverse <$> go [] c gr
  where
    go path c gr = do
      c' <- nextStep' c gr
      let gr' = pruneMore path c gr
      if c' == "end"
        then return $ c' : c : path
        else go (c : path) c' gr'

-- this part is a bit difficult and the duplicate check is uselessly repeated
-- when considering a longer path, but it does the job
pruneMore :: [Cave] -> Cave -> Graph -> Graph
pruneMore seen c gr
  | not (isSmall c) = gr
  | c `elem` ["start", "end"] = latestCavePruned
  | firstVisit && hasDupes = latestCavePruned
  | firstVisit = gr
  | otherwise = allPastSmallCavesPruned
  where
    firstVisit = c `notElem` seen
    hasDupes = any ((> 1) . length) . group . sort . filter isSmall $ seen
    latestCavePruned =
      snd . pruneGraph c $ gr
    allPastSmallCavesPruned =
      snd . foldr ((>=>) . pruneGraph) return (filter isSmall seen) $ gr

-- $> walk2 "start" $ buildGraph [("start","c"), ("c", "end"), ("c","B"), ("B","end")]

--- $> mapM_ (putStrLn . intercalate ",") . walk2 "start" $ testInput

solve2 :: Graph -> Int
solve2 = length . walk2 "start"

--- parsing

inputP :: Parser Graph
inputP = buildGraph <$> edgesP

edgesP :: Parser [Edge]
edgesP = sepEndBy edgeP endOfLine

edgeP :: Parser Edge
edgeP = liftA2 (,) (many1 letter <* char '-') (many1 letter)

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
