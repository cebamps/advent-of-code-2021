{-# OPTIONS_GHC -Wall #-}

module D23.Graph
  ( Dijkstra (SparseDijkstra),
    dijkstra,
  )
where

import Data.Bifunctor (first)
import qualified Data.Set as S

-- Implements the sparse version of Dijkstra's algorithm, assuming an integer
-- score.

type Score = Int

type Adjacency a = a -> [(Score, a)]

newtype Dijkstra a = SparseDijkstra
  { adjacent :: Adjacency a
  }

data SDState a = SDState
  { visited :: S.Set a,
    edge :: S.Set (Int, a)
  }
  deriving (Eq, Show)

dijkstra :: Ord a => Dijkstra a -> a -> a -> Int
dijkstra problem start end =
  let state = SDState {visited = S.empty, edge = S.singleton (0, start)}
   in dijkstra' end (adjacent problem) state

dijkstra' :: Ord a => a -> Adjacency a -> SDState a -> Int
dijkstra' end adj st = case popMinimum st of
  ((score, node), _) | node == end -> score
  ((score, x), st') -> do
    let ns = S.fromList $ first (+ score) <$> adj x
    dijkstra' end adj st' {edge = edge st' `S.union` ns}

-- mark it visited
-- expand edge to include neighbors, including their total score

-- takes the min node out of the edge, adds it to the visited set and returns
-- it
popMinimum :: Ord a => SDState a -> ((Score, a), SDState a)
popMinimum st =
  let (x, e') = deleteFindMinWhile ((`S.member` visited st) . snd) (edge st)
   in ( x,
        SDState
          { visited = snd x `S.insert` visited st,
            edge = e'
          }
      )

-- will error when called on the empty set
deleteFindMinWhile :: (a -> Bool) -> S.Set a -> (a, S.Set a)
deleteFindMinWhile predicate s = case S.deleteFindMin s of
  (x, s') | not (predicate x) -> (x, s')
  (_, s') -> deleteFindMinWhile predicate s'

{- ORMOLU_DISABLE -}
{- $>
  let sample :: Dijkstra Int
      sample = SparseDijkstra adj
        where
          adj 0 = [(1, 2), (4, 1)]
          adj 1 = [(6, 3), (1, 4)]
          adj 2 = [(1, 1)]
          adj 3 = [(1, 0)]
          adj 4 = []
          adj _ = undefined
   in dijkstra sample 0 4
<$ -}
{- ORMOLU_ENABLE -}
