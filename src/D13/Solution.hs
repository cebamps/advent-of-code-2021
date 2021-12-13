{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module D13.Solution (solve) where

import Control.Applicative (liftA2)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (foldl', groupBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (Sum, getSum))
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d13-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input
  putStrLn $ solve2 input

type Idx = (Int, Int)

-- I could use a Set but I don't know yet what part 2 will entail.
type Field a = M.Map Idx a

data Fold = FoldX Int | FoldY Int deriving (Eq, Show)

data Input = Input (Field Bool) [Fold] deriving (Show)

buildField :: [Idx] -> Field Bool
buildField = M.fromList . fmap (,True)

--- part 1

fold2D :: Fold -> Idx -> Idx
fold2D (FoldX fx) (x, y) = (fold1D fx x, y)
fold2D (FoldY fy) (x, y) = (x, fold1D fy y)

fold1D :: Int -> Int -> Int
fold1D fx x
  | x < fx = x
  | x > fx = 2 * fx - x
  | otherwise = error $ "folding across a point is not expected: " ++ show fx

foldFieldWith :: (a -> a -> a) -> Fold -> Field a -> Field a
foldFieldWith appendf fld = M.mapKeysWith appendf (fold2D fld)

-- $> testInput

-- comonads again!

solve1 :: Input -> Maybe Int
solve1 (Input _ []) = Nothing
solve1 (Input field (f : _)) = Just $ do
  let field' = foldFieldWith (&&) f field
  getSum $ foldMap (\x -> if x then Sum (1 :: Int) else mempty) field'

-- $> putStrLn $ solve2 testInput

--- part 2

-- visualize coordinates
printField :: [Idx] -> String
printField coords = unlines $ fmap printLine xcoords
  where
    xcoords :: [[Int]]
    xcoords = (fmap . fmap) fst . groupBy ((==) `on` snd) . sortOn snd $ coords

printLine :: [Int] -> String
printLine [] = ""
printLine (x : xs) = replicate x ' ' ++ '#' : printLine (subtract (x + 1) <$> xs)

solve2 :: Input -> String
solve2 (Input field folds) = do
  let field' = foldl' (flip $ foldFieldWith (&&)) field folds
  printField $ M.keys field'

--- parsing

inputP :: Parser Input
inputP = do
  field <- buildField <$> dotsP
  _ <- endOfLine
  Input field <$> foldsP

dotsP :: Parser [Idx]
dotsP = sepEndBy1 idxP endOfLine
  where
    idxP = liftA2 (,) (intP <* char ',') intP

intP :: Parser Int
intP = read <$> many1 digit

foldsP :: Parser [Fold]
foldsP = sepEndBy foldP endOfLine
  where
    foldP :: Parser Fold
    foldP = do
      _ <- string "fold along "
      constructor <- try (char 'x' $> FoldX) <|> char 'y' $> FoldY
      _ <- char '='
      constructor <$> intP

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
