{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module D22.Solution (solve) where

import Control.Monad (foldM, (>=>))
import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.List (foldl')
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d22-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input
  print $ solve2 input
  extraInfo input

type Input = [(Bool, Cube)]

data Axis = X | Y | Z deriving (Eq, Show)

-- expected (but not enforced) invariant: first <= second
type Range = (Int, Int)

data PlanePair = PlanePair Axis Range deriving (Eq, Show)

newtype Triple = Triple (Int, Int, Int) deriving (Eq, Show)

-- defines the extreme corners of the cube
data Cube = Cube Triple Triple deriving (Eq, Show)

type Cubes = Seq Cube

-- First time doing view patterns and pattern synonyms. This looks awful but
-- the resulting pattern synonym is nice. It turns out I could have defined my
-- data type like this to begin with, though -- I never use the actual
-- constructor.
--
-- For my own future reference: on the RHS of the "<-" is a view pattern, which
-- typically looks like (view -> pattern) where view is a function, and pattern
-- is matched on the output of that function. Confusingly, the view can be
-- inlined as a lambda, which makes the view pattern contain two right-pointing
-- arrows with different semantics.
--
-- Here, the view is the lambda taking a Cube and returning a 3-tuple of
-- 2-tuples, and the last arrow is the one in (view -> pattern) of the view
-- pattern syntax, matching the output of the view against the pattern (rx, ry,
-- rz).
--
-- We need an annotation to tell GHC that the pattern match succeeds for all
-- inputs, otherwise we get warnings.
{-# COMPLETE CubeRanges #-}

pattern CubeRanges :: Range -> Range -> Range -> Cube
pattern CubeRanges rx ry rz <-
  ( \(Cube (Triple (ax, ay, az)) (Triple (bx, by, bz))) ->
      ((ax, bx), (ay, by), (az, bz)) ->
      (rx, ry, rz)
    )
  where
    CubeRanges (ax, bx) (ay, by) (az, bz) = Cube (Triple (ax, ay, az)) (Triple (bx, by, bz))

cubeSize :: Cube -> Int
cubeSize (CubeRanges rx ry rz) = product $ fmap (\(a, b) -> 1 + b - a) [rx, ry, rz]

{-# ANN onCubeAxis "HLint: ignore Avoid lambda" #-}
-- reinventing lenses again!
onCubeAxis :: Functor f => Axis -> (Range -> f Range) -> Cube -> f Cube
onCubeAxis X f (CubeRanges xr yr zr) = (\xr' -> CubeRanges xr' yr zr) <$> f xr
onCubeAxis Y f (CubeRanges xr yr zr) = (\yr' -> CubeRanges xr yr' zr) <$> f yr
onCubeAxis Z f (CubeRanges xr yr zr) = (\zr' -> CubeRanges xr yr zr') <$> f zr

-- classic, see lens-tutorial
getCubeAxis :: Axis -> Cube -> Range
getCubeAxis ax = getConst . onCubeAxis ax Const

-- same thing
setCubeAxis :: Axis -> Range -> Cube -> Cube
setCubeAxis ax r = runIdentity . onCubeAxis ax (Identity . const r)

-- extracts the part of a cube situated between two planes, leaving behind the
-- other pieces
chop :: PlanePair -> Cube -> ([Cube], Maybe Cube)
chop (PlanePair ax range) c =
  -- ([Cube], Maybe Cube) is a functor on Cube, but I don't want to define a
  -- newtype wrapper for it just to use it more directly with my lens. Instead
  -- I explicitly compose getter and setter outside of the lens machinery.
  let (scrap, carved) = carve1D range $ getCubeAxis ax c
   in both (\r -> setCubeAxis ax r c) (scrap, carved)
  where
    both f (x, y) = (f <$> x, f <$> y)

-- $> chop (PlanePair X (0,5)) (CubeRanges (-1,7) (1,3) (10,20))

-- Subtracts the second cuboid from the first one, returning the subtraction as
-- a list of cuboids and the intersection, if any. A woodworking metaphor
-- helps: we carve out the intersection by working along successive axes: We
-- first cut the excess along X, then along Y, then along Z. The pieces of
-- scrap (0 to 2 along each axis, so a maximum of 6) are collected in the list.
carve :: Cube -> Cube -> ([Cube], Maybe Cube)
-- accumulates the scrap into the writer monad
carve c d = case (carveAlong X >=> carveAlong Y >=> carveAlong Z) $ Just c of
  -- Little optimization: we found out along the way that there is no
  -- intersection; glue the cube back together. This is a special case of a
  -- more general optimization that I won't bother with here, which would
  -- reorder the sequence of axes to minimize the number of scrap pieces.
  (_, Nothing) -> ([c], Nothing)
  x -> x
  where
    carveAlong :: Axis -> Maybe Cube -> ([Cube], Maybe Cube)
    carveAlong _ Nothing = return Nothing
    carveAlong ax (Just c') = chop (PlanePair ax (getCubeAxis ax d)) c'

-- $> carve (CubeRanges (0,2) (0,2) (0,3)) (CubeRanges (0,2) (0,2) (0,2))

-- $> carve (CubeRanges (0,3) (0,3) (0,3)) (CubeRanges (4,4) (1,1) (4,4))

{- ORMOLU_DISABLE -}
carve1D :: Range -> Range -> ([Range], Maybe Range)
-- 6 possibilities, though we will need to rearrange the equality cases a
-- little:
--  u < a        &  v < a,  a <= v <= b,  b < v
--  a <= u <= b  &          a <= v <= b,  b < v
--  b < u        &                        b < v
carve1D (u, v) r@(a, b)
  | v <  a || b <  u = ( [r]                     , Nothing     ) -- no intersection (two of the cases above)
  | u <= a && b <= v = ( []                      , Just r      ) -- full intersection
  | u <= a && v <  b = ( [(v + 1, b)]            , Just (a, v) ) -- intersection includes left extremity
  | a <  u && b <= v = ( [(a, u - 1)]            , Just (u, b) ) -- intersection includes right extremity
  | a <  u && v <  b = ( [(a, u - 1), (v + 1, b)], Just (u, v) ) -- intersection in the middle
  | otherwise = error "should not happen!"
{- ORMOLU_ENABLE -}

--- and now, the solution toolkit

countOn :: Cubes -> Int
countOn = sum . fmap cubeSize

minus :: Cube -> Cube -> Seq Cube
minus x = Seq.fromList . fst . carve x

-- Adds a cube to the cubes, removing overlaps. We could choose to do
-- otherwise, but we carve away at the cube being added in order to preserve
-- the existing set of cubes.
turnOn :: Cubes -> Cube -> Cubes
turnOn cs c = cs >< foldM minus c cs

-- Here we need to take out pieces of existing cuboids.
turnOff :: Cubes -> Cube -> Cubes
turnOff cs c = cs >>= (`minus` c)

-- $> countOn $ turnOff (Seq.fromList [CubeRanges (0,1) (0,1) (0,1)]) (CubeRanges (1,3) (1,3) (1,3))

condense :: Input -> Cubes
condense = foldl' act Seq.empty
  where
    act :: Seq Cube -> (Bool, Cube) -> Seq Cube
    act cs (True, c) = turnOn cs c
    act cs (False, c) = turnOff cs c

--- part 1: I figured I would need an efficient algorithm, so I am using it for
-- both parts

solve1 :: Input -> Int
solve1 input =
  let final = condense $ filter inPart1Range input
   in countOn final

inPart1Range :: (Bool, Cube) -> Bool
inPart1Range (_, CubeRanges xr yr zr) = inRange1 xr && inRange1 yr && inRange1 zr
  where
    inRange1 (a, b) = -50 <= a && b <= 50

--- part 2

solve2 :: Input -> Int
solve2 input =
  let final = condense input
   in countOn final

extraInfo :: Input -> IO ()
extraInfo input = do
  let final = condense input
  putStr "input cuboid count: "
  print $ length input
  putStr "final cuboid count: "
  print $ Seq.length final

--- parsing

inputP :: Parser Input
inputP = sepEndBy actionP endOfLine

actionP :: Parser (Bool, Cube)
actionP = (,) <$> onOrOffP <*> (char ' ' *> cubeP)
  where
    onOrOffP = char 'o' *> (True <$ string "n" <|> False <$ string "ff")

cubeP :: Parser Cube
cubeP = CubeRanges <$> coordP 'x' <*> (char ',' *> coordP 'y') <*> (char ',' *> coordP 'z')
  where
    coordP c = char c *> char '=' *> rangeP
    rangeP :: Parser Range
    rangeP = (,) <$> intP <*> (string ".." *> intP)
    intP :: Parser Int
    intP = fmap read $ option id ((:) <$> char '-') <*> many1 digit

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
