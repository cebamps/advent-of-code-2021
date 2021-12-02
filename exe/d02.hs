module Main where

import Data.Foldable (foldl')
import Input.D02 (Direction (..), input)

data Position = Position {horizP :: Int, depthP :: Int} deriving (Eq, Show)

data AimedPosition = AimedPosition {posAP :: Position, aimAP :: Int} deriving (Eq, Show)

initialPosition :: Position
initialPosition = Position 0 0

initialPositionA :: AimedPosition
initialPositionA = AimedPosition initialPosition 0

move :: Direction -> Position -> Position
move (Forward x) p = p {horizP = horizP p + x}
move (Up x) p = p {depthP = depthP p - x}
move (Down x) p = p {depthP = depthP p + x}

-- ah, this is screaming for lenses
moveA :: Direction -> AimedPosition -> AimedPosition
moveA (Forward x) p = p {posAP = (move (Forward x) . move (Down (x * aimAP p))) (posAP p)}
moveA (Up x) p = p {aimAP = aimAP p - x}
moveA (Down x) p = p {aimAP = aimAP p + x}

answer :: Position -> Int
answer (Position h d) = h * d

answerA :: AimedPosition -> Int
answerA (AimedPosition (Position h d) _) = h * d

-- Alternative: transform a list of commands from exercise 2 into a list of
-- commands from 1. Not very meaningful, but it works. The responsibility of
-- keeping up with state is split in two separate sequental halves though (aim
-- first, position second), which makes no sense but is a nice separation.

correctCommands :: [Direction] -> [Direction]
correctCommands = concatZipWith correctCommand . aims <*> id
  where
    concatZipWith f xs ys = concat $ zipWith f xs ys
    -- the only state we need to keep is the aim
    aims = scanl (+) 0 . fmap aimFromDirection
    aimFromDirection (Up x) = - x
    aimFromDirection (Down x) = x
    aimFromDirection _ = 0
    correctCommand aim (Forward x) = [Forward x, Down (aim * x)]
    correctCommand _ _ = []

main :: IO ()
main = do
  print $
    [ answer . foldl' (flip move) initialPosition,
      answerA . foldl' (flip moveA) initialPositionA,
      -- alternative solution to 2
      answer . foldl' (flip move) initialPosition . correctCommands
    ]
      <*> [input]
