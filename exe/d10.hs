module Main where

import D10.Solution (solve)
import qualified D10.Alt as Alt

main :: IO ()
main = do
  input <- getContents

  putStrLn "Main solution"
  solve input

  putStrLn "\nAlt solution"
  Alt.solve input
