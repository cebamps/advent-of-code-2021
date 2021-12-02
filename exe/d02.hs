module Main where

import Data.Foldable (foldl')
import Input.D02 (Direction (..), input)

data Position = Position {horizP :: Int, depthP :: Int} deriving (Eq, Show)

data FullPosition = FullPosition {posFP :: Position, aimFP :: Int} deriving (Eq, Show)

initialPosition :: Position
initialPosition = Position 0 0

initialPositionF :: FullPosition
initialPositionF = FullPosition initialPosition 0

move :: Direction -> Position -> Position
move (Forward x) p = p {horizP = horizP p + x}
move (Up x) p = p {depthP = depthP p - x}
move (Down x) p = p {depthP = depthP p + x}

-- ah, this is screaming for lenses
moveF :: Direction -> FullPosition -> FullPosition
moveF (Forward x) p =
  p
    { posFP =
        (posFP p)
          { horizP = (horizP . posFP) p + x,
            depthP = (depthP . posFP) p + aimFP p * x
          }
    }
moveF (Up x) p = p {aimFP = aimFP p - x}
moveF (Down x) p = p {aimFP = aimFP p + x}

answer :: Position -> Int
answer (Position h d) = h * d

answerF :: FullPosition -> Int
answerF (FullPosition (Position h d) _) = h * d

test :: FullPosition
test = foldl' (flip moveF) initialPositionF [Down 1, Forward 3, Up 4, Forward 1]

-- $> main

main :: IO ()
main = do
  print . answer . foldl' (flip move) initialPosition $ input
  print . answerF . foldl' (flip moveF) initialPositionF $ input
