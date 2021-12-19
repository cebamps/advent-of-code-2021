{-# OPTIONS_GHC -Wall #-}

-- Little pretty-printing typeclass inspired by Show to format things less
-- verbosely.
module D18.PPrint (PPrint(..)) where

import Data.List (intercalate)

class PPrint a where
  pprint :: a -> String
  pprintList :: [a] -> String
  pprintList xs = "[" ++ intercalate ", " (fmap pprint xs) ++ "]"

instance PPrint a => PPrint [a] where
  pprint = pprintList

instance PPrint Char where
  pprint = show
  pprintList = concatMap show

instance PPrint Int where pprint = show
