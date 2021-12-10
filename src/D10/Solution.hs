module D10.Solution (solve) where

import Data.Functor (($>))
import Data.List (find, foldl', sort)
import Data.Maybe (isJust, mapMaybe)
import Data.Monoid (First (First, getFirst))
import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe Data.Either

-- $> testInput = unsafePerformIO $ readFile "inputs/d10-test.txt" >>= (parseOrFail linesP)

-- terrible naming: chopping off the e from square so they all have the same
-- length :)
data Delimiter = Angle | Brace | Paren | Squar deriving (Eq, Show)

delimiters = [Angle, Brace, Paren, Squar]

delimiterPair :: Delimiter -> (Char, Char)
delimiterPair Angle = ('<', '>')
delimiterPair Brace = ('{', '}')
delimiterPair Paren = ('(', ')')
delimiterPair Squar = ('[', ']')

-- chunks contain zero or more chunks
data Chunk
  = Chunk Delimiter [Chunk]
  | BadChunk Delimiter Delimiter
  | UnfinishedChunk Delimiter [Chunk]
  deriving (Eq, Show)

-- there are one or more chunks on each line
type Line = [Chunk]

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail linesP inputStr
  print $ solve1 input
  print $ solve2 input

--- part 1

subChunks :: Chunk -> [Chunk]
subChunks (BadChunk _ _) = []
subChunks (Chunk _ cs) = cs
subChunks (UnfinishedChunk _ cs) = cs

-- This could be backed by a traversable tree instead, but I have baked the
-- tree structure into the datatype itself.
walkChunk :: Chunk -> [Chunk]
walkChunk c = c : (subChunks c >>= walkChunk)

walkLine :: Line -> [Chunk]
walkLine = concatMap walkChunk

firstBadChunk :: Line -> Maybe (Delimiter, Delimiter)
firstBadChunk = getFirst . foldMap (First . badChunkDelims) . walkLine
  where
    badChunkDelims (BadChunk ld rd) = Just (ld, rd)
    badChunkDelims _ = Nothing

-- $> mapM_ print . fmap firstBadChunk $ testInput

syntaxErrorScore :: Line -> Maybe Int
syntaxErrorScore = fmap scoreDelim . firstBadChunk
  where
    scoreDelim (_, Paren) = 3
    scoreDelim (_, Squar) = 57
    scoreDelim (_, Brace) = 1197
    scoreDelim (_, Angle) = 25137

solve1 :: [Line] -> Int
solve1 = sum . mapMaybe syntaxErrorScore

--- part 2

-- a good line has no bad chunks
isGoodLine :: Line -> Bool
isGoodLine = not . any isBadChunk . walkLine
  where
    isBadChunk (BadChunk _ _) = True
    isBadChunk _ = False

missingDelimiter :: Chunk -> Maybe Delimiter
missingDelimiter (UnfinishedChunk d _) = Just d
missingDelimiter _ = Nothing

-- the chunk tree is walked breadth-first, so we need to reverse to get the
-- deepest unfinished chunks first
missingDelimiters :: Line -> [Delimiter]
missingDelimiters = reverse . mapMaybe missingDelimiter . walkLine

completionScore :: [Delimiter] -> Int
completionScore = foldl' buildScore 0
  where
    buildScore s d = 5 * s + scoreDelim d
    scoreDelim Paren = 1
    scoreDelim Squar = 2
    scoreDelim Brace = 3
    scoreDelim Angle = 4

median :: Ord a => [a] -> a
median xsUnordered = let xs = sort xsUnordered in xs !! (length xs `div` 2)

solve2 :: [Line] -> Int
solve2 = median . fmap (completionScore . missingDelimiters) . filter isGoodLine

-- $> mapM_ print $ missingDelimiters <$> testInput

--- parsing. A lot happens here!

linesP :: Parser [Line]
linesP = sepEndBy lineP endOfLine <* eof

lineP :: Parser Line
lineP = many1 chunkP

chunkP :: Parser Chunk
chunkP = choice $ try . chunkDelimP <$> delimiters

closeP :: Parser Delimiter
closeP = choice $ try . closeDelimP <$> delimiters
  where
    closeDelimP :: Delimiter -> Parser Delimiter
    closeDelimP delim = char (snd . delimiterPair $ delim) $> delim

-- I considered writing a parser for the valid chunk case at first, but
-- conceptually, even if the narration of the exercise says that a mismatched
-- or missing delimiter is incorrect input, in our case it is correct input in
-- that we do things with it.
--
-- I am glad I approached it that way, because the second part required
-- consuming input beyond the first "error".
chunkDelimP :: Delimiter -> Parser Chunk
chunkDelimP delim = do
  let (l, r) = delimiterPair delim
  _ <- char l
  chunks <- many chunkP
  closeDelim <- optionMaybe closeP

  return $ case closeDelim of
    Just d | d == delim -> Chunk delim chunks
    Nothing -> UnfinishedChunk delim chunks
    Just closeDelim -> BadChunk delim closeDelim

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
