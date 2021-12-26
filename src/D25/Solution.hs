{-# OPTIONS_GHC -Wall #-}

module D25.Solution (solve) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Massiv.Array
  ( Array,
    B (B),
    Border (Wrap),
    Comp (Seq),
    Ix2 ((:.)),
    Stencil,
    Sz (Sz2),
    computeAs,
    fromLists',
    iterateUntil,
    makeStencil,
    mapStencil,
    toLists,
  )
import qualified Data.Massiv.Array as A
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d25-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input

type Field = Array B Ix2 Cell

data Cell = CucumberE | CucumberS | None deriving (Eq, Show, Enum, Bounded)

buildField :: [[Cell]] -> Field
buildField = fromLists' Seq

_showField :: Field -> String
_showField = unlines . (fmap . fmap) cellChar . toLists
  where
    cellChar None = '.'
    cellChar CucumberE = '>'
    cellChar CucumberS = 'v'

stepW :: Field -> Field
stepW = computeAs B . mapStencil Wrap st
  where
    st :: Stencil Ix2 Cell Cell
    st = makeStencil (Sz2 1 3) (0 :. 1) $ \f -> ruleW (f $ 0 :. (-1)) (f $ 0 :. 0) (f $ 0 :. 1)

stepS :: Field -> Field
stepS = computeAs B . mapStencil Wrap st
  where
    st :: Stencil Ix2 Cell Cell
    st = makeStencil (Sz2 3 1) (1 :. 0) $ \f -> ruleS (f $ (-1) :. 0) (f $ 0 :. 0) (f $ 1 :. 0)

ruleW :: Cell -> Cell -> Cell -> Cell
ruleW CucumberE None _ = CucumberE
ruleW _ CucumberE None = None
ruleW _ x _ = x

ruleS :: Cell -> Cell -> Cell -> Cell
ruleS CucumberS None _ = CucumberS
ruleS _ CucumberS None = None
ruleS _ x _ = x

--- part 1

-- This is awful! Since Massiv does not return the number of iterations but
-- still provides it to the convergence condition and stepper functions, we use
-- IO to mutate a counter every time the stepper function is called.
iterateUntilHack :: (Int -> Field -> Field -> Bool) -> (Int -> Field -> Field) -> Field -> (Int, Field)
iterateUntilHack conv next f = unsafePerformIO $ do
  iteration <- newIORef 0
  let next' it f' = unsafePerformIO $ do
        writeIORef iteration it >> return (next it f')
  let iterationResult = iterateUntil conv next' f
  lastIteration <- iterationResult `seq` readIORef iteration
  return (lastIteration, iterationResult)

solve1 :: Field -> Int
solve1 = (1 +) . fst . iterateUntilHack (const (==)) (const $ stepS . stepW)

--- parsing

inputP :: Parser Field
inputP = buildField <$> sepEndBy1 (many1 cellP) endOfLine

cellP :: Parser Cell
cellP = None <$ char '.' <|> CucumberE <$ char '>' <|> CucumberS <$ char 'v'

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
