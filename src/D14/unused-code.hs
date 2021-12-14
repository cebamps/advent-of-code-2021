-- ended up not needing those after realizing the problem would grow
-- exponentially
{-
type PairRules = M.Map Pair [Pair]

lookupPair :: Rules -> Pair -> [Pair]
lookupPair rules p = M.findWithDefault [p] p pairRules
  where
    pairRules = pairMap rules

replacePairs :: Rules -> [Pair] -> [Pair]
replacePairs rules = concatMap (lookupPair rules)

unzipPairs :: [(a, a)] -> [a]
unzipPairs [] = []
unzipPairs ((x, y) : ps) = x : y : (snd <$> ps)

-- optimization to avoid unpacking/repacking pairs in a left scan
replaceN :: Int -> Rules -> String -> String
replaceN n rules = unzipPairs . ((!! n) . iterate (replacePairs rules)) . zipPairs
-}

