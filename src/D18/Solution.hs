{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module D18.Solution (solve) where

import Control.Monad (forM_)
import D18.PPrint (PPrint (..))
import D18.Utility (unfoldId, whileJust)
import Data.Bifunctor (Bifunctor (second))
import Data.List (foldl1')
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d18-test.txt" >>= parseOrFail inputP

-- Snailfish numbers are binary trees with values at the leaves. The ordering
-- matters if we want to talk about "left" and "right" nodes.
--
-- I will likely need something like a zipper to perform localized updates of
-- the tree. Exploding a pair requires walking up the tree to get to the two
-- leaves on the left and right. There is no limit to how far up we will need
-- to walk to reach those leaves: they may not have all their ancestors in
-- common -- in fact we may need to go up to the root.
--
-- One thing I wonder is, is snailfish addition associative? Probably not,
-- because numbers can drop out if they are at the edge of the list, and them
-- being at the edge can change by adding things on the left or right. Let's
-- see:
--
-- let k = [0, [0, [0, [0, 1]]]]
--     z = [0, 0]
--
-- k + z
-- = [[0, [0, [0, [0, 1]]]], [0, 0]]
-- = [[0, [0, [0, 0]]], [1, 0]]
--
-- z + k
-- = [[0, 0], [0, [0, [0, [0, 1]]]]]
-- = [[0, 0], [0, [0, [0, 0]]]]
--
-- z + (k + z)
-- = [[0, 0], [[0, [0, [0, 0]]], [1, 0]]]
-- = [[0, 0], [[0, [0, 0]], [1, 0]]]
--
-- (z + k) + z
-- = [[[0, 0], [0, [0, [0, 0]]]], [0, 0]]
-- = [[[0, 0], [0, [0, 0]]], [0, 0]]
--
-- Yup, not the same thing!

-- OK, let's get started with a mass of definitions

-- your usual leafy binary tree
data Tree a = Leaf a | Pair (Tree a) (Tree a) deriving (Eq, Show)

instance PPrint a => PPrint (Tree a) where
  pprint (Leaf x) = pprint x
  pprint (Pair x y) = "[" ++ pprint x ++ ", " ++ pprint y ++ "]"

mapLeftmost :: (a -> a) -> Tree a -> Tree a
mapLeftmost f (Leaf x) = Leaf $ f x
mapLeftmost f (Pair l r) = Pair (mapLeftmost f l) r

mapRightmost :: (a -> a) -> Tree a -> Tree a
mapRightmost f (Leaf x) = Leaf $ f x
mapRightmost f (Pair l r) = Pair l (mapRightmost f r)

-- symmetric Either
data LR a = L a | R a deriving (Eq, Show)

instance PPrint a => PPrint (LR a) where
  pprint (L x) = "L " ++ pprint x
  pprint (R x) = "R " ++ pprint x

getLR :: LR a -> a
getLR (L x) = x
getLR (R x) = x

-- Zipper: contains a subtree, plus its sibling, plus its parent's sibling,
-- plus... Each sibling is marked as Left or Right so we have all the
-- information we need to reconstruct the whole tree. Borrowing infix
-- constructor name from Tarmo Uustalu so I have a reference to go back to if I
-- get lost. https://cs.ioc.ee/~tarmo/tsem05/uustalu0812-slides.pdf
data Zip a = [LR (Tree a)] :=| Tree a deriving (Eq, Show)

instance PPrint a => PPrint (Zip a) where
  pprint (p :=| t) = pprint p ++ " :=| " ++ pprint t

zipIsL :: Zip a -> Bool
zipIsL ((L _ : _) :=| _) = True
zipIsL _ = False

zipIsR :: Zip a -> Bool
zipIsR ((R _ : _) :=| _) = True
zipIsR _ = False

toZip :: Tree a -> Zip a
toZip t = [] :=| t

fromZip :: Zip a -> Tree a
fromZip (_ :=| t) = t

-- focuses the parent (if any), stating where we came from
zipUp :: Zip a -> Maybe (LR (Zip a))
zipUp ([] :=| _) = Nothing
zipUp ((L r : upwards) :=| t) = let t' = Pair t r in Just . L $ upwards :=| t'
zipUp ((R l : upwards) :=| t) = let t' = Pair l t in Just . R $ upwards :=| t'

-- focuses the sibling if there is one; no-op on a leaf
zipSide :: Zip a -> Zip a
zipSide ((L r : upwards) :=| l) = (R l : upwards) :=| r
zipSide ((R l : upwards) :=| r) = (L r : upwards) :=| l
zipSide z = z

-- transforms the subtree under focus
zipModify :: (Tree a -> Tree a) -> Zip a -> Zip a
zipModify f (path :=| t) = path :=| f t

-- zip back up all the way
zipFully :: Zip a -> Tree a
zipFully = zipFullyWith (fmap getLR . zipUp)
  where
    zipFullyWith :: (Zip a -> Maybe (Zip a)) -> Zip a -> Tree a
    zipFullyWith f = fromZip . whileJust f

-- get zipped children
zipChildren :: Zip a -> [Zip a]
zipChildren (_ :=| Leaf _) = []
zipChildren (path :=| Pair l r) = [zl :=| l, zr :=| r]
  where
    zl = L r : path
    zr = R l : path

-- produce a list of all zips, depth-first
walkZip :: Tree a -> [Zip a]
walkZip = go . toZip
  where
    go :: Zip a -> [Zip a]
    go zt = zt : concatMap go (zipChildren zt)

---

type SnailfishNumber = Tree Int

type Input = [SnailfishNumber]

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input
  print $ solve2 input

--- part 1: using zippers for snailfish math

data Rule a = Explode (Zip a) | Split (Zip a) deriving (Eq, Show)

type SnailfishRule = Rule Int

toRule :: Zip Int -> Maybe SnailfishRule
toRule z@(p :=| (Pair (Leaf _) (Leaf _))) | length p > 3 = Just $ Explode z
toRule z@(_ :=| Leaf x) | x >= 10 = Just $ Split z
toRule _ = Nothing

-- First-like Monoid encoding rule priorities
newtype FirstRule a = FirstRule {getFirstRule :: Maybe (Rule a)} deriving (Eq, Show)

instance Semigroup (FirstRule a) where
  FirstRule Nothing <> x = x
  (FirstRule (Just (Split _))) <> f@(FirstRule (Just (Explode _))) = f
  x <> _ = x

instance Monoid (FirstRule a) where
  mempty = FirstRule Nothing

findFirstRule :: SnailfishNumber -> Maybe SnailfishRule
findFirstRule = getFirstRule . foldMap (FirstRule . toRule) . walkZip

-- the split operation, with focus on the leaf to split
zipSplit :: Zip Int -> SnailfishNumber
zipSplit (path :=| (Leaf x)) = zipFully (path :=| splitToPair x)
  where
    splitToPair :: Int -> Tree Int
    splitToPair y = let (q, r) = y `divMod` 2 in Pair (Leaf q) (Leaf (q + r))
zipSplit _ = error "Can only split a regular number"

-- the explode operation, with focus on the pair to explode
zipExplode :: Zip Int -> SnailfishNumber
zipExplode (path :=| Pair (Leaf ex) (Leaf ey)) = go (Just ex, Just ey) (path :=| Leaf 0)
  where
    -- takes the numbers we must add on the left and right, walks upward and,
    -- for each of those two numbers, add them into the correct subtree as soon
    -- as we get our hands on it
    go :: (Maybe Int, Maybe Int) -> Zip Int -> SnailfishNumber
    -- we came in from the right, so we can add x to the (left) sibling
    go (Just x, my) z | zipIsR z = go (Nothing, my) . zipSide . zipModify (mapRightmost (+ x)) . zipSide $ z
    -- we came in from the left, so we can add y to the (right) sibling
    go (mx, Just y) z | zipIsL z = go (mx, Nothing) . zipSide . zipModify (mapLeftmost (+ y)) . zipSide $ z
    -- we're done modifying trees, go all the way up
    go (Nothing, Nothing) z = zipFully z
    go n z = case getLR <$> zipUp z of
      -- we have reached the parent, numbers will fall out
      Nothing -> fromZip z
      -- we have no immediate sibling to add our number to, continue walking
      Just z' -> go n z'
zipExplode _ = error "Can only explode a pair of regular numbers"

reduceOnce :: SnailfishNumber -> Maybe SnailfishNumber
reduceOnce t = do
  rule <- findFirstRule t
  return $ case rule of
    Explode z -> zipExplode z
    Split z -> zipSplit z

reduceIter :: SnailfishNumber -> [SnailfishNumber]
reduceIter = unfoldId reduceOnce

snailfishPlus :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
snailfishPlus t1 t2 = whileJust reduceOnce $ Pair t1 t2

magnitude :: SnailfishNumber -> Int
magnitude (Leaf x) = x
magnitude (Pair x y) = 3 * magnitude x + 2 * magnitude y

-- $> fmap magnitude <$> parseOrFail inputP "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"

solve1 :: Input -> Int
solve1 = magnitude . foldl1' snailfishPlus

-- debugging
_printReductions :: String -> IO ()
_printReductions s = do
  ts <- parseOrFail inputP s
  forM_ ts $ \t -> do
    mapM_ (putStrLn . pprint) $ reduceIter t
    putStrLn ""

-- debugging
_printCumSum :: Input -> IO ()
_printCumSum ts = mapM_ (putStrLn . pprint) $ scanl1 snailfishPlus ts

--- $> _printReductions "[[[[[9,8],1],2],3],4]"

--- $> _printReductions "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"

-- $> _printReductions "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"

-- $> _printCumSum testInput

--- part 2. Phew, this one is not too evil!

removeEach :: [a] -> [(a, [a])]
removeEach [] = []
removeEach (x : xs) = (x, xs) : (second (x :) <$> removeEach xs)

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x, xr) <- removeEach xs, y <- xr]

solve2 :: Input -> Int
solve2 = maximum . fmap (magnitude . uncurry snailfishPlus) . pairs

--- parsing

inputP :: Parser Input
inputP = sepEndBy pairP endOfLine

treeP :: Parser SnailfishNumber
treeP = pairP <|> leafP

leafP :: Parser SnailfishNumber
leafP = Leaf <$> intP
  where
    intP :: Parser Int
    intP = read <$> many1 digit

pairP :: Parser SnailfishNumber
pairP = Pair <$> (char '[' *> treeP) <*> (char ',' *> treeP) <* char ']'

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
