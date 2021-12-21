{-# OPTIONS_GHC -Wall #-}

module D21.Solution (solve) where

import Control.Arrow
import Data.List (find)
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d21-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input
  print $ solve2 input

--- part 1

type Input = (Int, Int)

data Player = P1 | P2 deriving (Eq, Show)

playerRoll :: Player -> Int -> Int
playerRoll P1 turn = 18 * turn + 6
playerRoll P2 turn = 18 * turn + 15

playerPos :: Player -> Int -> Int -> Int
playerPos p pos turn = 1 + (pos - 1 + playerRoll p turn) `mod` 10

playerPositions :: Player -> Int -> [Int]
playerPositions p initPos = go (playerPos p) initPos 0
  where
    go jmp pos turn = let nextPos = jmp pos turn in nextPos : go jmp nextPos (turn + 1)

scores :: (Int, Int) -> [(Int, Int)]
scores (p1pos, p2pos) =
  drop 1 $
    zip
      (cumsum $ playerPositions P1 p1pos)
      (cumsum $ playerPositions P2 p2pos)
  where
    cumsum = scanl (+) 0

alternatingScores :: (Int, Int) -> [(Player, Int)]
alternatingScores = go . scores
  where
    go [] = []
    go ((s1, s2) : ss) = (P1, s1) : (P2, s2) : go ss

findWithIndex :: (a -> Bool) -> [a] -> Maybe (Int, a)
findWithIndex p = find (p . snd) . zip [0 ..]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ tail xs

solve1 :: Input -> Int
solve1 input = do
  -- findWithIndex gets us the index of the previous element to the loser. with
  -- zero indexing, this means we need to add 2 and multiply by 3 to get the
  -- total rolls after the winner
  let Just (loserTurn, ((_, losingScore), _)) = findWithIndex ((>= 1000) . snd . snd) . pairs $ alternatingScores input
  let nRolls = (loserTurn + 2) * 3
  losingScore * nRolls

-- $> take 4 $ drop 327 $ zip [0..] $ alternatingScores testInput

--- part 2. Welp, we get no code reuse for once!

-- can be set to 2 for debugging
part2WinScore :: Int
part2WinScore = 21

--
-- see README.md for a lengthy comment
--

type Position = Int

type Score = Int

type Universes = M.Map (Score, Position) Int

evolveUniverses :: Universes -> Universes
evolveUniverses = M.fromListWith (+) . concatMap outcomes . M.assocs
  where
    outcomes :: ((Score, Position), Int) -> [((Score, Position), Int)]
    outcomes ((s, p), k) =
      [ ((s', p'), k * n)
        | (offset, n) <- universeSplits,
          let p' = 1 + (p + offset - 1) `mod` 10,
          let s' = s + p'
      ]
    -- see attached notes
    universeSplits =
      [ (3, 1), -- 1 J^3
        (4, 3), -- 3 J^4
        (5, 6), -- 6 J^5
        (6, 7), -- 7 J^6
        (7, 6), -- 6 J^7
        (8, 3), -- 3 J^8
        (9, 1) --  1 J^9
      ]

postSelect :: Universes -> (Int, Universes)
postSelect u =
  let (continuingU, stoppedU) = M.spanAntitone ((< part2WinScore) . fst) u
      stoppedCount = M.foldl' (+) 0 stoppedU
   in (stoppedCount, continuingU)

data PlayerState = PlayerState {pWon :: Int, pUniverses :: Universes} deriving (Eq, Show)

data GameState = GameState {p1State :: PlayerState, p2State :: PlayerState} deriving (Eq, Show)

pUniverseCount :: PlayerState -> Int
pUniverseCount = M.foldl' (+) 0 . pUniverses

stepPlayerState :: (PlayerState, PlayerState) -> PlayerState
stepPlayerState (otherP, PlayerState {pUniverses = u, pWon = won}) =
  let (wonInc, u') = (postSelect . evolveUniverses) u
      multiplicity = pUniverseCount otherP
   in PlayerState {pWon = won + multiplicity * wonInc, pUniverses = u'}

endGame :: PlayerState -> Bool
endGame = M.null . pUniverses

-- I guess this is where lenses come in handy. The player state update function
-- receives the other player and current player as a tuple, in that order.
updateGameState ::
  Functor f =>
  ((PlayerState, PlayerState) -> f PlayerState) ->
  (Player -> GameState -> f GameState)
updateGameState f P1 gs = (\s -> gs {p1State = s}) <$> f ((p2State &&& p1State) gs)
updateGameState f P2 gs = (\s -> gs {p2State = s}) <$> f ((p1State &&& p2State) gs)

-- fancy! Arrows and Functors working their magic
stepGameState :: Player -> GameState -> (Bool, GameState)
stepGameState = updateGameState $ (endGame &&& id) . stepPlayerState

initialGameState :: Input -> GameState
initialGameState (p1Pos, p2Pos) = GameState (initialPlayerState p1Pos) (initialPlayerState p2Pos)

initialUniverse :: Int -> Universes
initialUniverse pos = M.singleton (0, pos) 1

initialPlayerState :: Int -> PlayerState
initialPlayerState = PlayerState 0 . initialUniverse

runGame :: GameState -> GameState
runGame = go P1
  where
    go p gs = case stepGameState p gs of
      (True, gs') -> gs'
      (False, gs') -> go (nextPlayer p) gs'
    nextPlayer P1 = P2
    nextPlayer P2 = P1

-- $> iterate evolveUniverses (initialUniverse 1) !! 1

-- $> iterate evolveUniverses (initialUniverse 1) !! 2

solve2 :: Input -> Int
solve2 input = do
  let gs = runGame $ initialGameState input
  let wins = (pWon . p1State &&& pWon . p2State) gs
  uncurry max wins

--- parsing

inputP :: Parser Input
inputP = (,) <$> lineP <*> (endOfLine *> lineP)
  where
    lineP :: Parser Int
    lineP = string "Player " *> oneOf "12" *> string " starting position: " *> (read <$> many1 digit)

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
