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

main :: IO ()
main = do
  print $
    [ answer . foldl' (flip move) initialPosition,
      answerA . foldl' (flip moveA) initialPositionA
    ]
      <*> [input]
