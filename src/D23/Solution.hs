{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module D23.Solution (solve) where

import D23.Graph (Dijkstra (SparseDijkstra), dijkstra)
import Data.Bifunctor (first)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Tuple (swap)
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d23-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input
  print $ solve2 input

-- This is another Dijkstra problem, isn't it?

-- Rule 3 seems important, it restricts the graph quite a bit.

-- Rule 1 seems redundant with the other two: if one amphipod stops outside of
-- a room, it is either its unoccupied destination room (in which case it might
-- as well move in) or not (in which case it could have stopped one step
-- earlier, i.e., stayed inside instead of getting out and stopping, or stopped
-- between rooms to allow more movement). In both cases the energy expenditure
-- is lower and the set of future states more open.

-- There are 7 hallway spots we need to deal with, and 8 room spots.

data Amphipod = A | B | C | D deriving (Eq, Ord, Show)

-- Constructor: (room number) (deep)
-- Room numbers are 1-indexed to help alignment with hallway spots: room x is
-- connected to hallway spots x and x+1.
data Slot = Room Int Bool | Hallway Int deriving (Eq, Ord, Show)

type State = M.Map Slot Amphipod

finalState :: State
finalState =
  M.fromList
    [ (Room r d, a)
      | (r, a) <- [(1, A), (2, B), (3, C), (4, D)],
        d <- [True, False]
    ]

-- closest legal moves, characterizing the weighted graph of slots (before
-- applying the amphipod cost factor)
slotEdges :: Slot -> [(Int, Slot)]
slotEdges (Room x False) = [(2, Hallway x), (2, Hallway (x + 1)), (1, Room x True)]
slotEdges (Room x True) = [(1, Room x False)]
slotEdges (Hallway 0) = [(1, Hallway 1)]
slotEdges (Hallway 6) = [(1, Hallway 5)]
slotEdges (Hallway 1) = [(1, Hallway 0), (2, Hallway 2)] ++ hallwayRooms 1
slotEdges (Hallway 5) = [(1, Hallway 6), (2, Hallway 4)] ++ hallwayRooms 5
slotEdges (Hallway x) = [(2, Hallway (x - 1)), (2, Hallway (x + 1))] ++ hallwayRooms x

hallwayRooms :: Int -> [(Int, Slot)]
hallwayRooms 1 = [(2, Room 1 False)]
hallwayRooms 5 = [(2, Room 4 False)]
hallwayRooms h = [(2, Room r False) | r <- [h - 1, h]]

amphipodCost :: Amphipod -> Int
amphipodCost A = 1
amphipodCost B = 10
amphipodCost C = 100
amphipodCost D = 1000

amphipodRoom :: Amphipod -> Int
amphipodRoom A = 1
amphipodRoom B = 2
amphipodRoom C = 3
amphipodRoom D = 4

-- Is the amphipod in this slot in its final position. It does not matter
-- whether the amphipod was removed from the state.
isFinalSlot :: State -> (Slot, Amphipod) -> Bool
isFinalSlot _ (Room x True, amph) = x == amphipodRoom amph
isFinalSlot st (Room x False, amph) =
  x == amphipodRoom amph
    && maybe False ((x ==) . amphipodRoom) (st M.!? Room x True)
isFinalSlot _ _ = False

-- Find all the targets we can walk to from one spot in a given state. We don't
-- need Dijkstra for this: the shortest path is also the most direct one,
-- despite having some shortcuts of higher cost baked in the transitions, as we
-- have no slot in front of a room.
floodFill :: State -> Slot -> [(Int, Slot)]
floodFill st slot = go M.empty [(0, slot)]
  where
    go :: M.Map Slot Int -> [(Int, Slot)] -> [(Int, Slot)]
    go visited [] = swap <$> M.assocs visited
    go visited queue =
      let queue' = filter ((\s -> M.notMember s visited && M.notMember s st) . snd) $ concatMap jump queue
          visited' = visited `M.union` M.fromList (swap <$> queue)
       in go visited' queue'
    jump :: (Int, Slot) -> [(Int, Slot)]
    jump (x, s) = first (+ x) <$> slotEdges s

-- Determine the slots an amphipod can move to and insert it there. It does not
-- matter whether the amphipod was removed first, but if it wasn't, there will
-- be a duplicate amphipod to remove from the output.
amphipodTransitions :: State -> (Slot, Amphipod) -> [(Int, State)]
amphipodTransitions st (s, amph) = (fmap . fmap) insert . filter (allowed . snd) $ floodFill st s
  where
    allowed :: Slot -> Bool
    allowed target = case (s, target) of
      -- rules 2 and 3: an amphipod in the hallway can only go to its final spot
      (Hallway _, Room _ _) -> isFinalSlot st (target, amph)
      (Room _ _, Hallway _) -> True
      _ -> False
    insert target = M.insert target amph st

-- We only have to consider the closest legal moves: transitions can compose.
-- If we jumped spots we would have to filter out transitions that jump over
-- amphipods.
transitions :: State -> [(Int, State)]
transitions st =
  [ (amphipodCost amph * cost, st')
    | (slot, amph) <- M.assocs st,
      (cost, st') <- amphipodTransitions (M.delete slot st) (slot, amph)
  ]

printState :: State -> String
printState st =
  unlines
    [ replicate 13 '#',
      "#" ++ ph 0 ++ intercalate "." [ph x | x <- [1 .. 5]] ++ ph 6 ++ "#",
      "###" ++ intercalate "#" [pr0 x | x <- [1 .. 4]] ++ "###",
      "  #" ++ intercalate "#" [pr1 x | x <- [1 .. 4]] ++ "#",
      "  " ++ replicate 9 '#'
    ]
  where
    ph = printSlot . Hallway
    pr0 = printSlot . flip Room False
    pr1 = printSlot . flip Room True
    printSlot = maybe "." show . (M.!?) st

printStateIO :: State -> IO ()
printStateIO = putStr . printState

--- part 1

solve1 :: State -> Int
solve1 st = dijkstra (SparseDijkstra transitions) st finalState

--- part 2: I need a little break, will go back to this later. It will
-- take a small refactoring in how I deal with rooms, but should be quick
-- enough.

solve2 :: a -> ()
solve2 = const ()

--- parsing

inputP :: Parser State
inputP = fmap M.fromList $ do
  _ <- string (replicate 13 '#') <* end
  h <- hallwayP <* end
  r1 <- roomP False <* end
  r2 <- roomP True <* end
  _ <- string ("  " ++ replicate 9 '#')
  return $ h ++ r1 ++ r2
  where
    end :: Parser ()
    end = skipMany (char ' ') <* endOfLine

hallwayP :: Parser [(Slot, Amphipod)]
hallwayP = do
  _ <- char '#'
  h1 <- count 2 slotP
  h2 <- count 4 (char '.' *> slotP)
  h3 <- count 1 slotP
  _ <- char '#'
  let hs = zipWith (\h a -> (Hallway h,) <$> a) [0 .. 6] $ h1 ++ h2 ++ h3
  return $ catMaybes hs

roomP :: Bool -> Parser [(Slot, Amphipod)]
roomP deep = do
  _ <- string $ if deep then "  " else "##"
  rs <- count 4 (char '#' *> slotP)
  _ <- string $ if deep then "#" else "###"
  return . catMaybes $ zipWith (\r a -> (Room r deep,) <$> a) [1 .. 4] rs

--- $> testInput

slotP :: Parser (Maybe Amphipod)
slotP =
  Nothing <$ char '.'
    <|> Just A <$ char 'A'
    <|> Just B <$ char 'B'
    <|> Just C <$ char 'C'
    <|> Just D <$ char 'D'

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
