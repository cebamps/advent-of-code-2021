{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative (ZipList (ZipList, getZipList), liftA2)
import Data.Foldable (toList)
import Data.List (find, partition, transpose)
import Data.Monoid (Endo (..), First (..))
import Input.D04

-- general purpose

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (l, r) = splitAt n xs in l : chunk n r

composeAll :: [a -> a] -> a -> a
composeAll = appEndo . foldMap Endo

composeMap :: (b -> (a -> a)) -> [b] -> a -> a
composeMap f = composeAll . fmap f

--

-- consumes rows of five in chunks of five to produce a set of boards
mkBoards :: [[Int]] -> [Board Int]
mkBoards = fmap (mkBoard . concat) . chunk 5

inputBoards = mkBoards . snd $ input

testBoards = mkBoards . snd $ testInput

inputMarks = fst input

testMarks = fst testInput

--

newtype Board a = Board [a] deriving (Eq, Show, Functor)

data MarkedBoard a = MarkedBoard {mbmarks :: Board Bool, mbboard :: Board a} deriving (Eq, Show, Functor)

instance Applicative Board where
  pure x = mkBoard (replicate 25 x)
  Board fs <*> Board xs = Board . getZipList $ ZipList fs <*> ZipList xs

instance Foldable Board where
  foldr f q (Board xs) = foldr f q xs

mkBoard :: [a] -> Board a
mkBoard xs
  | length xs == 25 = Board xs
  | otherwise = error "Invalid board"

mkmBoard :: Board a -> MarkedBoard a
mkmBoard = MarkedBoard (pure False)

mark :: Eq a => a -> MarkedBoard a -> MarkedBoard a
mark x m@MarkedBoard {mbmarks, mbboard} =
  let newmarks = liftA2 ((||) . (== x)) mbboard mbmarks
   in m {mbmarks = newmarks}

isWon :: MarkedBoard a -> Bool
isWon MarkedBoard {mbmarks} = (or . fmap and) rows || (or . fmap and) columns
  where
    rows = let Board bs = mbmarks in chunk 5 bs
    columns = transpose rows

splitBoard :: MarkedBoard a -> ([a], [a])
splitBoard MarkedBoard {mbmarks, mbboard} = let (ts, fs) = partition fst tuples in (snd <$> ts, snd <$> fs)
  where
    tuples = toList $ liftA2 (,) mbmarks mbboard

score :: Num a => a -> MarkedBoard a -> a
score lastDraw board = let (_, unmarked) = splitBoard board in sum unmarked * lastDraw

showBoard' :: (a -> String) -> Board a -> String
showBoard' show' (Board xs) = do
  let strs = show' <$> xs
  let maxWidth = maximum $ length <$> strs
  let paddedStrs = fmap padLeft strs
        where
          padLeft s = replicate (maxWidth - length s) ' ' ++ s
  unlines . fmap unwords . chunk 5 $ paddedStrs

showBoard :: Show a => Board a -> String
showBoard = showBoard' show

showMarkedBoard' :: (a -> String) -> MarkedBoard a -> String
showMarkedBoard' show' (MarkedBoard m b) = showBoard' id $ fmap showMark m <*> fmap show' b
  where
    showMark False xs = " " ++ xs ++ " "
    showMark True xs = "[" ++ xs ++ "]"

showMarkedBoard :: Show a => MarkedBoard a -> String
showMarkedBoard = showMarkedBoard' show

--- $> let b = mark 8 . mark 22 . mark 21 . mark 6 . mark 1 . mkmBoard $ head testBoards
--- $>   in (putStrLn . showMarkedBoard) b >> (print . isWon) b

markSequence :: Eq a => MarkedBoard a -> [a] -> [MarkedBoard a]
markSequence = scanl (flip mark)

--- $> let s = markSequence (mkmBoard $ head testBoards) testMarks
--- $>   in mapM_ (\x -> putStrLn (showMarkedBoard x) >> putStrLn ("^-" ++ show (isWon x))) s

markSequenceMulti :: Eq a => [MarkedBoard a] -> [a] -> [[MarkedBoard a]]
markSequenceMulti = scanl (flip $ fmap . mark)

findWin :: Eq a => [MarkedBoard a] -> [a] -> Maybe (a, MarkedBoard a)
findWin boards marks = getFirst $ foldMap First wonSeq
  where
    markSeq = drop 1 $ markSequenceMulti boards marks
    wonBoardSeq = find isWon <$> markSeq
    wonSeq = zipWith (\m b -> (m,) <$> b) marks wonBoardSeq

answer1 :: (Eq a, Num a) => [Board a] -> [a] -> Maybe a
answer1 boards marks = do
  let mboards = mkmBoard <$> boards
  (lastMark, winnerBoard) <- findWin mboards marks
  return $ score lastMark winnerBoard

-- part 2

wins :: Eq a => [MarkedBoard a] -> [a] -> [(a, MarkedBoard a)]
wins _ [] = []
wins boards (step : steps) = case partition isWon (mark step <$> boards) of
  (winners, losers) -> fmap (step,) winners ++ wins losers steps

-- we could write answer1 similarly
answer2 :: (Eq a, Num a) => [Board a] -> [a] -> Maybe a
answer2 boards marks = do
  let mboards = mkmBoard <$> boards
  let (lastMark, winnerBoard) = last $ wins mboards marks
  return $ score lastMark winnerBoard

---

main :: IO ()
main = do
  putStr "Test: "
  print $ answer1 testBoards testMarks
  putStr "Actual: "
  print $ answer1 inputBoards inputMarks
  putStrLn ""

  putStr "Test: "
  print $ answer2 testBoards testMarks
  putStr "Actual: "
  print $ answer2 inputBoards inputMarks
