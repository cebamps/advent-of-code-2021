{-# LANGUAGE LambdaCase #-}

module Input.D02 (Direction (..), input) where

data Direction
  = Forward Int
  | Down Int
  | Up Int
  deriving (Eq, Show)

parseDirection :: String -> Int -> Direction
parseDirection = \case
  "forward" -> Forward
  "down" -> Down
  "up" -> Up
  unrecognized -> error $ "error parsing direction " ++ unrecognized

parseValue :: String -> Int
parseValue = read

parseLine :: String -> Direction
parseLine line = case words line of
  [dirStr, valueStr] -> parseDirection dirStr $ parseValue valueStr
  _ -> error $ "error parsing line: " ++ line

input :: [Direction]
input = fmap parseLine . lines $ inputString

inputString =
  "\
  \forward 9\n\
  \forward 7\n\
  \forward 4\n\
  \down 7\n\
  \forward 5\n\
  \down 4\n\
  \down 2\n\
  \forward 1\n\
  \down 6\n\
  \forward 5\n\
  \forward 5\n\
  \forward 8\n\
  \forward 3\n\
  \forward 6\n\
  \down 2\n\
  \up 3\n\
  \forward 1\n\
  \up 4\n\
  \forward 1\n\
  \forward 6\n\
  \up 2\n\
  \forward 7\n\
  \up 2\n\
  \up 3\n\
  \down 9\n\
  \up 5\n\
  \down 5\n\
  \up 7\n\
  \down 5\n\
  \forward 4\n\
  \forward 1\n\
  \forward 8\n\
  \forward 9\n\
  \forward 3\n\
  \forward 9\n\
  \down 1\n\
  \down 1\n\
  \down 1\n\
  \forward 6\n\
  \up 8\n\
  \down 2\n\
  \forward 3\n\
  \down 9\n\
  \forward 7\n\
  \down 5\n\
  \up 7\n\
  \down 3\n\
  \forward 5\n\
  \forward 9\n\
  \down 9\n\
  \up 3\n\
  \forward 4\n\
  \forward 8\n\
  \up 2\n\
  \forward 4\n\
  \forward 2\n\
  \forward 2\n\
  \forward 8\n\
  \up 7\n\
  \up 4\n\
  \down 3\n\
  \forward 2\n\
  \down 9\n\
  \up 1\n\
  \forward 2\n\
  \down 3\n\
  \forward 2\n\
  \down 2\n\
  \up 6\n\
  \forward 6\n\
  \forward 2\n\
  \down 9\n\
  \forward 3\n\
  \forward 2\n\
  \down 1\n\
  \up 1\n\
  \down 1\n\
  \forward 5\n\
  \forward 4\n\
  \up 6\n\
  \forward 6\n\
  \forward 2\n\
  \up 9\n\
  \forward 9\n\
  \forward 5\n\
  \down 8\n\
  \up 9\n\
  \forward 2\n\
  \up 3\n\
  \forward 8\n\
  \forward 2\n\
  \down 4\n\
  \down 1\n\
  \up 9\n\
  \up 8\n\
  \forward 3\n\
  \forward 9\n\
  \down 9\n\
  \down 6\n\
  \forward 1\n\
  \forward 5\n\
  \up 9\n\
  \down 7\n\
  \up 9\n\
  \down 8\n\
  \down 2\n\
  \down 2\n\
  \up 9\n\
  \forward 7\n\
  \down 4\n\
  \down 7\n\
  \down 8\n\
  \down 9\n\
  \down 9\n\
  \forward 9\n\
  \down 9\n\
  \forward 2\n\
  \down 6\n\
  \forward 8\n\
  \forward 1\n\
  \down 6\n\
  \down 8\n\
  \forward 5\n\
  \forward 3\n\
  \forward 5\n\
  \down 5\n\
  \forward 6\n\
  \forward 3\n\
  \forward 4\n\
  \forward 3\n\
  \down 2\n\
  \down 9\n\
  \down 8\n\
  \down 9\n\
  \up 7\n\
  \up 9\n\
  \up 1\n\
  \down 6\n\
  \down 4\n\
  \forward 8\n\
  \forward 5\n\
  \forward 8\n\
  \down 5\n\
  \forward 7\n\
  \down 9\n\
  \forward 9\n\
  \forward 1\n\
  \up 4\n\
  \down 4\n\
  \forward 7\n\
  \forward 4\n\
  \up 9\n\
  \forward 6\n\
  \down 8\n\
  \down 5\n\
  \forward 8\n\
  \down 6\n\
  \down 3\n\
  \down 7\n\
  \forward 4\n\
  \down 8\n\
  \down 1\n\
  \up 6\n\
  \down 4\n\
  \down 9\n\
  \down 6\n\
  \forward 7\n\
  \down 8\n\
  \forward 5\n\
  \forward 8\n\
  \down 7\n\
  \down 5\n\
  \forward 1\n\
  \up 1\n\
  \down 1\n\
  \down 6\n\
  \forward 5\n\
  \forward 6\n\
  \down 1\n\
  \up 6\n\
  \forward 4\n\
  \forward 6\n\
  \down 4\n\
  \forward 3\n\
  \up 6\n\
  \forward 2\n\
  \forward 9\n\
  \down 8\n\
  \forward 8\n\
  \up 9\n\
  \forward 3\n\
  \forward 4\n\
  \forward 1\n\
  \down 4\n\
  \down 5\n\
  \forward 4\n\
  \down 6\n\
  \forward 7\n\
  \down 7\n\
  \down 8\n\
  \up 8\n\
  \up 5\n\
  \down 8\n\
  \down 5\n\
  \forward 8\n\
  \forward 2\n\
  \forward 6\n\
  \up 5\n\
  \forward 4\n\
  \forward 2\n\
  \forward 1\n\
  \up 7\n\
  \forward 1\n\
  \forward 6\n\
  \down 8\n\
  \down 5\n\
  \down 4\n\
  \forward 2\n\
  \up 8\n\
  \forward 4\n\
  \up 8\n\
  \forward 7\n\
  \forward 5\n\
  \down 4\n\
  \up 7\n\
  \down 5\n\
  \down 3\n\
  \forward 2\n\
  \down 2\n\
  \down 2\n\
  \forward 5\n\
  \forward 2\n\
  \down 2\n\
  \forward 5\n\
  \down 8\n\
  \forward 7\n\
  \up 8\n\
  \down 6\n\
  \up 5\n\
  \forward 6\n\
  \up 9\n\
  \down 2\n\
  \down 3\n\
  \up 1\n\
  \up 8\n\
  \forward 9\n\
  \forward 7\n\
  \forward 9\n\
  \forward 3\n\
  \down 2\n\
  \up 2\n\
  \down 2\n\
  \down 8\n\
  \up 8\n\
  \up 6\n\
  \forward 6\n\
  \down 9\n\
  \down 9\n\
  \up 4\n\
  \down 3\n\
  \forward 6\n\
  \forward 9\n\
  \down 6\n\
  \forward 7\n\
  \forward 4\n\
  \forward 4\n\
  \down 9\n\
  \down 3\n\
  \forward 1\n\
  \down 7\n\
  \forward 2\n\
  \forward 3\n\
  \forward 9\n\
  \forward 5\n\
  \forward 2\n\
  \forward 4\n\
  \forward 8\n\
  \up 1\n\
  \forward 5\n\
  \down 4\n\
  \down 2\n\
  \down 7\n\
  \forward 1\n\
  \up 1\n\
  \up 8\n\
  \up 6\n\
  \down 1\n\
  \forward 1\n\
  \forward 9\n\
  \forward 8\n\
  \down 7\n\
  \forward 6\n\
  \forward 8\n\
  \down 7\n\
  \forward 5\n\
  \down 5\n\
  \down 8\n\
  \down 8\n\
  \forward 8\n\
  \up 1\n\
  \down 7\n\
  \down 4\n\
  \up 4\n\
  \forward 5\n\
  \up 7\n\
  \forward 3\n\
  \forward 2\n\
  \down 1\n\
  \forward 3\n\
  \down 5\n\
  \forward 4\n\
  \down 4\n\
  \forward 6\n\
  \up 9\n\
  \forward 3\n\
  \down 7\n\
  \forward 7\n\
  \forward 9\n\
  \forward 9\n\
  \forward 4\n\
  \up 9\n\
  \up 5\n\
  \down 6\n\
  \down 6\n\
  \forward 8\n\
  \up 6\n\
  \down 2\n\
  \up 5\n\
  \forward 7\n\
  \forward 4\n\
  \down 6\n\
  \down 4\n\
  \down 9\n\
  \down 4\n\
  \up 2\n\
  \down 3\n\
  \down 7\n\
  \forward 1\n\
  \forward 4\n\
  \down 6\n\
  \forward 3\n\
  \forward 2\n\
  \forward 4\n\
  \down 9\n\
  \forward 8\n\
  \down 3\n\
  \up 4\n\
  \down 5\n\
  \forward 2\n\
  \down 6\n\
  \forward 8\n\
  \down 8\n\
  \down 7\n\
  \down 4\n\
  \forward 1\n\
  \down 3\n\
  \forward 9\n\
  \down 2\n\
  \down 9\n\
  \down 2\n\
  \forward 1\n\
  \down 3\n\
  \down 2\n\
  \down 2\n\
  \up 4\n\
  \down 8\n\
  \forward 6\n\
  \forward 4\n\
  \forward 4\n\
  \up 9\n\
  \forward 3\n\
  \forward 1\n\
  \forward 1\n\
  \up 3\n\
  \forward 9\n\
  \down 2\n\
  \forward 5\n\
  \down 9\n\
  \down 2\n\
  \forward 1\n\
  \forward 9\n\
  \down 3\n\
  \forward 3\n\
  \up 3\n\
  \forward 7\n\
  \down 6\n\
  \up 8\n\
  \down 2\n\
  \down 5\n\
  \forward 7\n\
  \down 8\n\
  \up 5\n\
  \down 4\n\
  \up 5\n\
  \forward 6\n\
  \forward 3\n\
  \down 2\n\
  \forward 4\n\
  \forward 3\n\
  \down 8\n\
  \forward 5\n\
  \forward 5\n\
  \down 5\n\
  \forward 1\n\
  \forward 8\n\
  \up 1\n\
  \down 7\n\
  \forward 6\n\
  \forward 3\n\
  \forward 8\n\
  \down 9\n\
  \down 7\n\
  \forward 1\n\
  \down 2\n\
  \down 6\n\
  \down 3\n\
  \forward 8\n\
  \down 7\n\
  \forward 2\n\
  \forward 1\n\
  \forward 5\n\
  \down 9\n\
  \forward 2\n\
  \forward 2\n\
  \up 4\n\
  \down 9\n\
  \down 4\n\
  \forward 7\n\
  \down 7\n\
  \up 8\n\
  \forward 6\n\
  \down 9\n\
  \down 8\n\
  \up 5\n\
  \down 8\n\
  \down 6\n\
  \forward 9\n\
  \up 5\n\
  \up 7\n\
  \down 3\n\
  \up 2\n\
  \down 4\n\
  \up 8\n\
  \up 3\n\
  \down 7\n\
  \forward 9\n\
  \forward 7\n\
  \down 7\n\
  \forward 5\n\
  \up 8\n\
  \forward 1\n\
  \down 2\n\
  \forward 8\n\
  \down 3\n\
  \up 5\n\
  \down 9\n\
  \forward 8\n\
  \down 7\n\
  \down 3\n\
  \down 3\n\
  \down 2\n\
  \forward 6\n\
  \up 5\n\
  \forward 4\n\
  \down 4\n\
  \down 3\n\
  \down 5\n\
  \forward 8\n\
  \down 3\n\
  \forward 7\n\
  \forward 2\n\
  \down 8\n\
  \down 6\n\
  \down 9\n\
  \down 3\n\
  \down 6\n\
  \down 7\n\
  \down 8\n\
  \up 6\n\
  \down 7\n\
  \forward 8\n\
  \down 9\n\
  \forward 1\n\
  \down 6\n\
  \forward 8\n\
  \down 5\n\
  \forward 3\n\
  \up 8\n\
  \forward 1\n\
  \down 6\n\
  \forward 4\n\
  \forward 5\n\
  \forward 8\n\
  \up 5\n\
  \forward 4\n\
  \down 2\n\
  \down 9\n\
  \up 2\n\
  \forward 1\n\
  \up 8\n\
  \forward 6\n\
  \up 4\n\
  \up 6\n\
  \forward 4\n\
  \up 5\n\
  \forward 6\n\
  \forward 1\n\
  \down 3\n\
  \down 6\n\
  \up 2\n\
  \forward 4\n\
  \up 2\n\
  \forward 4\n\
  \forward 6\n\
  \down 2\n\
  \down 4\n\
  \up 5\n\
  \down 9\n\
  \up 2\n\
  \down 4\n\
  \up 6\n\
  \forward 3\n\
  \down 6\n\
  \down 2\n\
  \up 8\n\
  \down 3\n\
  \down 1\n\
  \forward 6\n\
  \forward 5\n\
  \forward 8\n\
  \down 4\n\
  \down 6\n\
  \down 2\n\
  \forward 3\n\
  \down 3\n\
  \up 8\n\
  \down 4\n\
  \forward 5\n\
  \down 6\n\
  \down 3\n\
  \up 2\n\
  \forward 5\n\
  \forward 2\n\
  \down 6\n\
  \down 8\n\
  \forward 1\n\
  \forward 5\n\
  \forward 7\n\
  \forward 3\n\
  \forward 6\n\
  \down 9\n\
  \forward 7\n\
  \forward 4\n\
  \down 6\n\
  \down 2\n\
  \up 8\n\
  \down 3\n\
  \down 7\n\
  \down 7\n\
  \down 9\n\
  \down 8\n\
  \down 6\n\
  \down 6\n\
  \up 1\n\
  \up 6\n\
  \forward 4\n\
  \down 8\n\
  \up 7\n\
  \down 8\n\
  \forward 9\n\
  \down 9\n\
  \up 9\n\
  \forward 4\n\
  \forward 1\n\
  \down 3\n\
  \down 8\n\
  \forward 9\n\
  \down 9\n\
  \forward 3\n\
  \down 2\n\
  \forward 9\n\
  \down 2\n\
  \forward 8\n\
  \down 7\n\
  \down 2\n\
  \forward 4\n\
  \forward 3\n\
  \forward 3\n\
  \down 8\n\
  \up 3\n\
  \forward 9\n\
  \down 1\n\
  \down 6\n\
  \up 3\n\
  \down 6\n\
  \up 7\n\
  \forward 9\n\
  \up 9\n\
  \down 5\n\
  \forward 6\n\
  \up 1\n\
  \up 6\n\
  \down 4\n\
  \forward 9\n\
  \forward 6\n\
  \forward 9\n\
  \down 4\n\
  \up 9\n\
  \up 4\n\
  \forward 2\n\
  \forward 2\n\
  \forward 4\n\
  \up 6\n\
  \down 1\n\
  \down 4\n\
  \forward 9\n\
  \down 9\n\
  \forward 3\n\
  \up 9\n\
  \down 4\n\
  \forward 4\n\
  \down 1\n\
  \forward 8\n\
  \forward 2\n\
  \down 1\n\
  \down 7\n\
  \down 8\n\
  \forward 1\n\
  \up 7\n\
  \up 7\n\
  \forward 1\n\
  \down 3\n\
  \up 5\n\
  \down 4\n\
  \forward 2\n\
  \down 5\n\
  \up 1\n\
  \down 4\n\
  \forward 7\n\
  \down 2\n\
  \down 5\n\
  \down 4\n\
  \forward 7\n\
  \forward 6\n\
  \up 9\n\
  \forward 6\n\
  \forward 1\n\
  \forward 7\n\
  \forward 5\n\
  \up 6\n\
  \down 8\n\
  \forward 8\n\
  \down 9\n\
  \down 8\n\
  \forward 8\n\
  \down 2\n\
  \down 5\n\
  \forward 8\n\
  \forward 9\n\
  \down 6\n\
  \down 3\n\
  \down 3\n\
  \up 9\n\
  \down 6\n\
  \forward 6\n\
  \up 2\n\
  \forward 9\n\
  \forward 7\n\
  \forward 6\n\
  \forward 4\n\
  \forward 1\n\
  \down 2\n\
  \forward 1\n\
  \forward 3\n\
  \forward 9\n\
  \down 9\n\
  \forward 7\n\
  \forward 3\n\
  \down 8\n\
  \up 7\n\
  \forward 1\n\
  \down 8\n\
  \up 5\n\
  \down 8\n\
  \up 3\n\
  \down 7\n\
  \forward 2\n\
  \down 7\n\
  \forward 2\n\
  \down 3\n\
  \forward 3\n\
  \forward 8\n\
  \down 4\n\
  \forward 6\n\
  \down 3\n\
  \up 9\n\
  \forward 9\n\
  \up 6\n\
  \up 4\n\
  \up 6\n\
  \down 1\n\
  \forward 3\n\
  \down 7\n\
  \down 9\n\
  \up 9\n\
  \down 2\n\
  \up 6\n\
  \forward 4\n\
  \down 4\n\
  \down 3\n\
  \down 2\n\
  \down 6\n\
  \forward 1\n\
  \forward 1\n\
  \up 3\n\
  \forward 5\n\
  \forward 8\n\
  \down 1\n\
  \up 4\n\
  \forward 3\n\
  \up 4\n\
  \down 5\n\
  \up 7\n\
  \down 5\n\
  \down 6\n\
  \forward 9\n\
  \forward 8\n\
  \forward 9\n\
  \down 6\n\
  \forward 5\n\
  \down 3\n\
  \up 5\n\
  \down 7\n\
  \down 5\n\
  \down 7\n\
  \up 9\n\
  \forward 3\n\
  \forward 4\n\
  \forward 1\n\
  \up 3\n\
  \forward 2\n\
  \down 4\n\
  \up 9\n\
  \down 7\n\
  \forward 6\n\
  \forward 5\n\
  \forward 3\n\
  \forward 3\n\
  \forward 9\n\
  \up 7\n\
  \down 9\n\
  \forward 4\n\
  \down 7\n\
  \forward 9\n\
  \forward 5\n\
  \down 8\n\
  \up 2\n\
  \forward 2\n\
  \down 4\n\
  \up 5\n\
  \up 4\n\
  \forward 5\n\
  \down 4\n\
  \down 9\n\
  \down 7\n\
  \down 2\n\
  \forward 1\n\
  \forward 1\n\
  \down 4\n\
  \down 8\n\
  \down 6\n\
  \forward 1\n\
  \up 6\n\
  \up 3\n\
  \up 5\n\
  \down 1\n\
  \down 5\n\
  \up 1\n\
  \up 5\n\
  \forward 2\n\
  \up 2\n\
  \down 3\n\
  \forward 7\n\
  \forward 2\n\
  \down 1\n\
  \down 9\n\
  \forward 1\n\
  \down 1\n\
  \forward 9\n\
  \up 9\n\
  \down 9\n\
  \forward 9\n\
  \down 4\n\
  \down 1\n\
  \up 5\n\
  \down 2\n\
  \forward 9\n\
  \down 2\n\
  \up 3\n\
  \up 6\n\
  \forward 1\n\
  \forward 8\n\
  \down 5\n\
  \down 8\n\
  \up 2\n\
  \down 2\n\
  \up 4\n\
  \down 2\n\
  \down 4\n\
  \forward 6\n\
  \up 4\n\
  \down 1\n\
  \forward 9\n\
  \forward 4\n\
  \down 9\n\
  \up 7\n\
  \forward 7\n\
  \down 3\n\
  \forward 2\n\
  \down 6\n\
  \up 6\n\
  \down 5\n\
  \down 7\n\
  \forward 4\n\
  \forward 1\n\
  \forward 7\n\
  \forward 4\n\
  \forward 4\n\
  \up 2\n\
  \down 2\n\
  \down 5\n\
  \forward 7\n\
  \down 6\n\
  \forward 8\n\
  \down 3\n\
  \down 9\n\
  \forward 7\n\
  \forward 1\n\
  \down 2\n\
  \up 7\n\
  \forward 4\n\
  \forward 2\n\
  \forward 6\n\
  \forward 5\n\
  \forward 9\n\
  \forward 9\n\
  \down 9\n\
  \down 9\n\
  \up 7\n\
  \forward 7\n\
  \forward 7\n\
  \forward 1\n\
  \forward 2\n\
  \down 1\n\
  \down 4\n\
  \forward 7\n\
  \forward 5\n\
  \down 1\n\
  \up 2\n\
  \forward 3\n\
  \forward 2\n\
  \forward 1\n\
  \forward 6\n\
  \down 4\n\
  \up 6\n\
  \forward 7\n\
  \down 1\n\
  \forward 4\n\
  \up 6\n\
  \down 7\n\
  \down 4\n\
  \forward 1\n\
  \down 8\n\
  \down 2\n\
  \down 1\n\
  \down 8\n\
  \forward 4\n\
  \up 8\n\
  \down 4\n\
  \up 9\n\
  \up 3\n\
  \forward 6\n\
  \up 9\n\
  \down 1\n\
  \forward 3\n\
  \up 3\n\
  \forward 5\n\
  \up 3\n\
  \down 6\n\
  \forward 9\n\
  \down 3\n\
  \down 3\n\
  \up 5\n\
  \forward 5\n\
  \forward 8\n\
  \forward 9\n\
  \down 6\n\
  \down 3\n\
  \forward 6\n\
  \up 4\n\
  \up 3\n\
  \forward 3\n\
  \forward 2\n\
  \down 2\n\
  \up 9\n\
  \forward 4\n\
  \forward 6\n\
  \forward 2\n\
  \up 9\n\
  \down 2\n\
  \forward 7\n\
  \down 7\n\
  \up 1\n\
  \forward 2\n\
  \forward 8\n\
  \down 2\n\
  \down 6\n\
  \down 1\n\
  \forward 3\n\
  \forward 5\n\
  \forward 6\n\
  \forward 3\n\
  \down 3\n\
  \down 7\n\
  \up 3\n\
  \forward 2\n\
  \forward 5\n\
  \down 9\n\
  \forward 3\n\
  \down 9\n\
  \up 6\n\
  \down 6\n\
  \forward 3\n\
  \down 5\n\
  \forward 1\n\
  \down 5\n\
  \up 3\n\
  \forward 8\n\
  \forward 8\n\
  \down 5\n\
  \down 6\n\
  \down 1\n\
  \forward 9\n\
  \forward 4\n\
  \forward 1\n\
  \forward 8\n\
  \down 8\n\
  \down 9\n\
  \forward 7\n\
  \forward 9\n\
  \down 2\n\
  \down 6\n\
  \down 8\n\
  \down 3\n\
  \forward 5\n\
  \forward 7\n\
  \forward 4\n\
  \down 9\n\
  \down 2\n\
  \forward 4\n\
  \forward 7\n\
  \down 2\n\
  \down 7\n\
  \forward 8\n\
  \down 8\n\
  \forward 4\n\
  \up 8\n\
  \forward 3\n\
  \forward 9\n\
  \forward 4\n\
  \down 9\n\
  \down 6\n\
  \up 1\n\
  \down 3\n\
  \down 7\n\
  \down 4\n\
  \forward 9\n\
  \forward 4\n\
  \up 9\n\
  \down 6\n\
  \forward 3\n\
  \up 1\n\
  \down 8\n\
  \down 5\n\
  \forward 9\n\
  \down 4\n\
  \down 2\n\
  \down 2\n\
  \down 5\n\
  \up 5\n\
  \down 5\n\
  \forward 5\n\
  \forward 2\n\
  \up 1\n\
  \forward 2\n\
  \up 2\n\
  \forward 8\n\
  \down 2\n\
  \down 7\n\
  \forward 1\n\
  \"
