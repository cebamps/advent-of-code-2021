{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module D20.Solution (solve) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d20-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input
  print $ solve2 input

type State = Bool

type Rule = V.Vector Bool

type Input = (Rule, Field)

type Idx = (Int, Int)

-- ambient state + particular bits
type Field = (State, M.Map Idx State)

buildRule :: [Bool] -> Rule
buildRule = V.fromList

buildField :: [(Idx, Bool)] -> Field
buildField elts = (False, M.fromList elts)

fromBools :: Num a => [Bool] -> a
fromBools = foldl' shift 0
  where
    shift s True = 2 * s + 1
    shift s False = 2 * s

rule :: Rule -> [State] -> State
rule r bs = r V.! fromBools bs

ambientRule :: Rule -> State -> State
ambientRule r False = r V.! 0
ambientRule r True = r V.! 511

-- order matters here!!
neighborhoodIdx :: Idx -> [Idx]
neighborhoodIdx (x, y) =
  [ (x', y')
    | y' <- [y - 1, y, y + 1],
      x' <- [x - 1, x, x + 1]
  ]

unique :: Ord a => [a] -> [a]
unique = S.toList . S.fromList

allNeighbors :: [Idx] -> [Idx]
allNeighbors = unique . concatMap neighborhoodIdx

-- this is reallocating the field a whole lot, hopefully that's not a problem
update :: Rule -> Field -> Field
update r (ambient, f) =
  let ambient' = ambientRule r ambient
   in (ambient',) $ do
        M.fromList
          [ (upd, val')
            | upd <- allNeighbors $ M.keys f,
              let updN = flip (M.findWithDefault ambient) f <$> neighborhoodIdx upd,
              let val' = rule r updN,
              -- optimization: discard ambient values to keep the map small
              val' /= ambient'
          ]

--- parts 1 and 2

solveN :: Int -> Input -> Int
solveN n (r, f) =
  let (_, final) = iterate (update r) f !! n
   in length . filter id $ M.elems final

solve1 :: Input -> Int
solve1 = solveN 2

solve2 :: Input -> Int
solve2 = solveN 50

--- parsing

inputP :: Parser Input
inputP = (,) <$> ruleP <*> (endOfLine *> fieldP) <* eof

ruleP :: Parser Rule
ruleP = buildRule <$> count 512 binP <* endOfLine

fieldP :: Parser Field
fieldP =
  buildField <$> do
    rows <- zip [0 ..] <$> sepEndBy1 rowP endOfLine
    return $
      [ ((x, y), s)
        | (y, row) <- rows,
          (x, s) <- row
      ]
  where
    rowP = zip [0 ..] <$> many1 binP

binP :: Parser Bool
binP = False <$ char '.' <|> True <$ char '#'

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
