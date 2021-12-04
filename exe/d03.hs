module Main where

import Control.Applicative (Applicative (liftA2), ZipList (ZipList, getZipList))
import Data.List (foldl', group, maximumBy, minimumBy, sort, transpose)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Ap (Ap, getAp), Sum (Sum))
import Data.Ord (comparing)
import Input.D03 (input, testInput)

newtype CombineMap k a = CombineMap (Map.Map k a) deriving (Eq, Show)

instance (Ord k, Semigroup a) => Semigroup (CombineMap k a) where
  CombineMap m1 <> CombineMap m2 = CombineMap $ Map.unionWith (<>) m1 m2

instance (Ord k, Semigroup a) => Monoid (CombineMap k a) where
  mempty = CombineMap Map.empty

type GammaBit = CombineMap Char (Sum Integer)

singletonCM :: k -> a -> CombineMap k a
singletonCM k x = CombineMap $ Map.singleton k x

lookupCM :: (Ord k, Monoid a) => k -> CombineMap k a -> a
lookupCM k (CombineMap m) = Map.findWithDefault mempty k m

toGamma :: Char -> GammaBit
toGamma = flip singletonCM (Sum 1)

-- Zips lists of Semigroup values together. Unlike fold, there is no empty
-- value so we wrap the outcome in Maybe.
zipFold :: (Foldable f, Semigroup m) => f [m] -> Maybe [m]
zipFold = fmap (getZipList . getAp) . foldMap (Just . Ap . ZipList)

-- not safe on empty lists
fromGamma :: GammaBit -> Char
fromGamma (CombineMap m) = fst . maximumBy (comparing snd) . Map.toList $ m

gammaBits :: [[Char]] -> Maybe [Char]
gammaBits = (fmap . fmap) fromGamma . zipFold . (fmap . fmap) toGamma

flipBit :: Char -> Char
flipBit = fromMaybe <*> flip lookup [('0', '1'), ('1', '0')]

epsilonBits :: [[Char]] -> Maybe [Char]
epsilonBits = (fmap . fmap) flipBit . gammaBits

bitsToInteger :: [Char] -> Integer
bitsToInteger = foldl' shiftAdd 0
  where
    shiftAdd x '1' = x * 2 + 1
    shiftAdd x _ = x * 2

--- part 2

-- transpose and keep suffixes

filterMask :: [Bool] -> [a] -> [a]
filterMask mask = catMaybes . zipWith (\x -> if x then Just else const Nothing) mask

-- unsafe on empty list
mostCommon :: (Ord a, Eq a) => [a] -> a
mostCommon = head . maximumBy (comparing length) . group . sort

-- unsafe on empty list
leastCommon :: (Ord a, Eq a) => [a] -> a
leastCommon = head . minimumBy (comparing length) . group . sort

locate :: (Ord a, Eq a) => ([a] -> a) -> [[a]] -> [a]
locate crit = locate' crit . transpose

-- works on the transposed list: computes the prefix we want to keep from the
-- first list, stash it away in the recursion output, and filters the
-- subsequent lists to keep only the ones that come from that prefix
locate' :: (Ord a, Eq a) => ([a] -> a) -> [[a]] -> [a]
locate' crit (xs : xss) =
  let x = crit xs
      mask = fmap (== x) xs
      filtered = fmap (filterMask mask) xss
   in x : locate' crit filtered
-- could use a short circuit when xs is x:[]
locate' _ [] = []

main :: IO ()
main = do
  let gamma = bitsToInteger <$> gammaBits input
  let epsilon = bitsToInteger <$> epsilonBits input
  print $ liftA2 (*) gamma epsilon

  let o2Rating = (bitsToInteger . locate mostCommon) input
  let co2Rating = (bitsToInteger . locate leastCommon) input
  print $ o2Rating * co2Rating
