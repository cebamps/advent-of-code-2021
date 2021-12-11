{-# LANGUAGE LambdaCase #-}

module D10.Alt (solve) where

import Control.Monad ((>=>))
import Data.Either (lefts, rights)
import Data.List (foldl', sort)

-- the mismatched closing character
type Error = Char

-- the stack of expected closing characters
type Stack = [Char]

type ProcessState = Either Error Stack

push :: Char -> Stack -> ProcessState
push x xs = Right (x : xs)

pop :: Char -> Stack -> ProcessState
pop x = \case
  x' : xs | x' == x -> return xs
  _ -> Left x

process :: [Char] -> ProcessState
process xs = process' xs []

-- Compose a bunch of Kleisli arrows left to right; foldr for early exit. This
-- one takes an initial stack as argument.
process' :: [Char] -> Stack -> ProcessState
process' = foldr ((>=>) . op) return
  where
    op '[' = push ']'
    op '(' = push ')'
    op '<' = push '>'
    op '{' = push '}'
    op x = pop x

scoreStack :: Stack -> Int
scoreStack = foldl' (\s x -> 5 * s + closingScore x) 0
  where
    closingScore ')' = 1
    closingScore ']' = 2
    closingScore '}' = 3
    closingScore '>' = 4
    closingScore _ = 0

scoreError :: Error -> Int
scoreError = mismatchScore
  where
    mismatchScore ')' = 3
    mismatchScore ']' = 57
    mismatchScore '}' = 1197
    mismatchScore '>' = 25137
    mismatchScore _ = 0

score :: ProcessState -> Int
score = either scoreError scoreStack

-- $> score . process $ "[({(<(())[]>[[{[]{<()<>>"

-- $> score . process $ "[[<[([]))<([[{}[[()]]]"

solve :: String -> IO ()
solve input = do
  print . solve1 . lines $ input
  print . solve2 . lines $ input

solve1 :: [String] -> Int
solve1 = sum . fmap scoreError . lefts . fmap process

solve2 :: [String] -> Int
solve2 = median . fmap scoreStack . rights . fmap process
  where
    median :: Ord a => [a] -> a
    median xs = sort xs !! (length xs `div` 2)
