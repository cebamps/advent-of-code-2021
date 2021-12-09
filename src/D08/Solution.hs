{-# LANGUAGE TupleSections #-}

module D08.Solution (solve) where

import Control.Monad (foldM)
import Data.Char (toUpper)
import Data.Either (partitionEithers)
import Data.List (delete, foldl', permutations, sort, sortOn, (\\))
import Data.Maybe (listToMaybe)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)

{-

  aaaa            aaaa    aaaa
 b    c       c       c       c  b    c
 b    c       c       c       c  b    c
                  dddd    dddd    dddd
 e    f       f  e            f       f
 e    f       f  e            f       f
  gggg            gggg    gggg

  aaaa    aaaa    aaaa    aaaa    aaaa
 b       b            c  b    c  b    c
 b       b            c  b    c  b    c
  dddd    dddd            dddd    dddd
      f  e    f       f  e    f       f
      f  e    f       f  e    f       f
  gggg    gggg            gggg    gggg

A wire is a character from 'a' to 'g'.

A rewiring is a permutation of ['a'..'g'].

A rewired digit is an unordered collection of wires. The digit rewired under
the identity permutation is a special case.

A sample is a set of rewired digits.

A trial is a sample of 10 paired with a sample of 4.

-}

data Wire = A | B | C | D | E | F | G deriving (Eq, Ord, Enum, Bounded, Read, Show)

type Digit = [Wire]

type Sample = [Digit]

type Trial = (Sample, Sample)

solve :: String -> IO ()
solve inputStr = do
  input <- parseInput inputStr

  validateHypothesis input
  putStrLn ""

  print $ solve1 input
  print $ solve2 input

validateHypothesis :: [Trial] -> IO ()
validateHypothesis input = do
  putStr "all digits exist in sample of 10 (unordered): "
  print $ all validateOneSorted input
  putStr "all digits exist in sample of 10 (ordered):   "
  print $ all validateOne input
  where
    validateOne (sample, value) = all (`elem` sample) value
    validateOneSorted (sample, value) = all (`elem` fmap sort sample) (fmap sort value)

--- part 1 is easy

sumOn :: Num b => (a -> b) -> [a] -> b
sumOn f = sum . fmap f

solve1 :: [Trial] -> Int
solve1 = sumOn $ length . filter ((`elem` [2, 4, 7, 3]) . length) . snd

--- part 2

-- starting with some definitions and permutation utilities

type PartialPermutation = [(Wire, Wire)]

-- used for lookup and to recognize digits that are legal
allCandidateDigits :: [(Digit, Int)]
allCandidateDigits =
  [ ([A, B, C, E, F, G], 0),
    ([C, F], 1),
    ([A, C, D, E, G], 2),
    ([A, C, D, F, G], 3),
    ([B, C, D, F], 4),
    ([A, B, D, F, G], 5),
    ([A, B, D, E, F, G], 6),
    ([A, C, F], 7),
    ([A, B, C, D, E, F, G], 8),
    ([A, B, C, D, F, G], 9)
  ]

-- returns (untouched, touched)
permute :: PartialPermutation -> [Wire] -> ([Wire], [Wire])
permute p wires = partitionEithers permuted
  where
    permuteOne :: PartialPermutation -> Wire -> Either Wire Wire
    permuteOne p w = maybe (Left w) Right $ lookup w p
    permuted = permuteOne p <$> wires

-- builds all permutations from the a set to the b set; lengths work as with
-- zip
makePermutations :: [a] -> [b] -> [[(a, b)]]
makePermutations xs ys = fmap (zip xs) (permutations ys)

-- And now the core part.
--
-- Start with a scrambled digit with n segments. Find all candidate digits. For
-- each candidate, we have one partial permutation, which maps the scrambled
-- segments to the candidate. For n = 2, the only candidate is 1 and we have
-- two permutations. For n = 8, we also have one candidate, but 7! = 5040
-- permutations. In general, smaller segment counts yield less
-- solution-permutation pairs:
--
-- 2 -> [1]       -> 1 * 2! =    2
-- 3 -> [7]       -> 1 * 3! =    6
-- 4 -> [4]       -> 1 * 4! =   24
-- 5 -> [2, 3, 5] -> 3 * 5! =  360
-- 6 -> [0, 6, 9] -> 3 * 6! = 2160
-- 7 -> [8]       -> 1 * 7! = 5040
--
-- Say we now have a partial permutation. Take the next scrambled digit, with m
-- segments, and a candidate digit of m segments. Applying the partial
-- permutation to the scrambled digit yields a set of unscrambled segments and
-- a set of leftover unmapped segments. There are two possibilities:
--
--  - Some of the unscrambled segments are not in the candidate digit. This is
--    a dead end: we have a bad permutation.
--  - The unscrambled segments all reside in the candidate digit. Yield the
--    partial permutation augmented with all partial permutations mapping the
--    unmapped segments to the remaining segments of the candidate.
--
-- Hence, from one partial permutation, we branch into legal candidate digits,
-- and for each legal candidate digit, we branch into an extension of the
-- partial permutation.
--
-- Dealing with partial permutations means we don't have to look back for
-- digits we have already unscrambled. At each step the permutation resolves
-- all the previous scrambled digits into different, valid digits.

-- partial permutation up to now, remaining candidates
type Progress = (PartialPermutation, [Digit])

--
allowedPermutations :: Progress -> Digit -> [Progress]
allowedPermutations (p, candidates) scrambled = do
  let (leftoverSeg, unscrambledSeg) = permute p scrambled

  -- list monad magic: we creates branches of computation to consider each case
  candidate <- filter ((== length scrambled) . length) candidates

  -- UnscrambledSeg should all belong to the candidate. We are interested to
  -- know which parts of the candidate are not explained by the current
  -- permutation, so that we can grow the permutation to explain it fully.
  let leftoverFromCandidate = candidate \\ unscrambledSeg
  let incompatible = length leftoverFromCandidate /= length leftoverSeg

  if incompatible
    then [] -- branch dies off here
    else do
      -- More branches: grow the permutation such that the leftover segments of
      -- the scrambled digit unscramble to the leftover segments of the
      -- candidate.
      p' <- (p ++) <$> makePermutations leftoverSeg leftoverFromCandidate
      return (p', delete candidate candidates)

-- it's better to sort the Digits by length here, as noted earlier: they
-- tend to create less branches
walkSolutions :: Progress -> [Digit] -> [Progress]
walkSolutions = foldM allowedPermutations

sortedDigits :: [Digit]
sortedDigits = sortOn length $ fst <$> allCandidateDigits

resolveDigit :: Digit -> Maybe Int
resolveDigit w = lookup (sort w) allCandidateDigits

digitsToInt :: [Int] -> Int
digitsToInt = foldl' (\a x -> 10 * a + x) 0

unscramble :: Trial -> Maybe Int
unscramble (sample, scrambledValue) = do
  let initialState = ([], sortedDigits)
  (p, []) <- listToMaybe . walkSolutions initialState . sortOn length $ sample

  let solvedValue = snd . permute p <$> scrambledValue

  digitsToInt <$> mapM resolveDigit solvedValue

solve2 :: [Trial] -> Maybe Int
solve2 = fmap sum . mapM unscramble

--- parsing

fromChar :: Char -> Maybe Wire
fromChar = readMaybe . (: []) . toUpper

inputParser :: Parser [Trial]
inputParser = do
  endBy trial endOfLine
  where
    wire :: Parser Wire
    wire = oneOf ['a' .. 'g'] >>= maybe (unexpected "invalid wire") return . fromChar
    sample :: Parser Sample
    sample = sepEndBy (many1 wire) (char ' ')
    trial :: Parser Trial
    trial = (,) <$> (sample <* skipMany (oneOf " |")) <*> sample

-- i won't bother learning EitherT for now
parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""

parseInput = parseOrFail inputParser
