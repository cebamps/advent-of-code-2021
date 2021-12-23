module Main where

import D15.Solution (solve)
import qualified D15.Alt as Alt

main :: IO ()
main = do
  input <- getContents
  putStrLn "Optimized solution using D23:"
  Alt.solve input

  putStrLn "\nOriginal solution:"
  solve input
