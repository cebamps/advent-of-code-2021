{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module D16.Solution (solve) where

import Control.Monad ((>=>))
import Data.Foldable (fold, foldl')
import Data.List (elemIndex, intercalate)
import Data.Monoid (Sum (Sum, getSum))
import Text.Parsec
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d16-test.txt" >>= parseOrFail inputP

-- header (bits)
-- xxxyyy
--
-- x: version
-- y: type id
--
-- types:
--  4: literal value (number): sequence of 5-bit groups, first bit encoding
--  continuation (1) or last word (0), four remaining concatenate to the binary
--  representation of the number
--
--  other: operator. Header followed by one bit
--

-- this sequencing is kind of weird, no?
parseInputOrFail :: String -> IO Token
parseInputOrFail = parseOrFail binInputP >=> parseOrFail packetP

solve :: String -> IO ()
solve inputStr = do
  input <- parseInputOrFail inputStr

  print $ solve1 input
  print $ solve2 input

  putStrLn "\nBonus"
  putStrLn $ printExpr . expr $ input

--- utilities for parsing

foldMapA :: (Applicative t, Monoid b, Traversable f) => (a -> t b) -> f a -> t b
foldMapA f = fmap fold . traverse f

hexDigits :: [Char]
hexDigits = ['0' .. '9'] ++ ['A' .. 'F']

hex2bin :: Char -> Maybe [Char]
hex2bin = fmap int2bin4 . flip elemIndex hexDigits
  where
    int2bin x = case x `divMod` 2 of
      (0, 0) -> []
      (x', 0) -> int2bin x' ++ "0"
      (x', _) -> int2bin x' ++ "1"
    int2bin4 x = let ds = int2bin x in replicate (4 - length ds) '0' ++ ds

bin2int :: [Char] -> Int
bin2int = foldl' shifter 0
  where
    shifter s '0' = 2 * s
    shifter s _ = 2 * s + 1

expandHex :: [Char] -> Maybe [Char]
expandHex = foldMapA hex2bin

data PacketMeta = PacketMeta {getVersion :: Int, getType :: Int} deriving (Show, Eq)

data Token
  = LiteralP PacketMeta Int
  | OperatorP PacketMeta Int [Token]
  deriving (Show, Eq)

-- after part 2
data Expr
  = Literal Int
  | OperationExpr Operation [Expr]
  deriving (Show, Eq)

data Operation
  = SumOp
  | ProdOp
  | MinOp
  | MaxOp
  | GtOp
  | LtOp
  | EqOp
  deriving (Show, Eq)

--- part 1

solve1 :: Token -> Int
solve1 = getSum . foldToken (Sum . getTokenVersion)
  where
    getTokenVersion = \case
      OperatorP meta _ _ -> getVersion meta
      LiteralP meta _ -> getVersion meta

foldToken :: Monoid m => (Token -> m) -> Token -> m
foldToken f x@(LiteralP _ _) = f x
foldToken f x@(OperatorP _ _ toks) = f x <> foldMap (foldToken f) toks

--- part 2

solve2 :: Token -> Int
solve2 = runExpr . expr

expr :: Token -> Expr
expr (LiteralP _ x) = Literal x
expr (OperatorP _ typeId ts) = ($ fmap expr ts) . OperationExpr $ case typeId of
  0 -> SumOp
  1 -> ProdOp
  2 -> MinOp
  3 -> MaxOp
  5 -> GtOp
  6 -> LtOp
  7 -> EqOp
  x -> error $ "unknown operator type " ++ show x

runExpr :: Expr -> Int
runExpr (Literal x) = x
runExpr (OperationExpr op xs) = runOp op $ runExpr <$> xs

runOp :: Operation -> [Int] -> Int
runOp SumOp xs = sum xs
runOp ProdOp xs = product xs
runOp MinOp xs = minimum xs
runOp MaxOp xs = maximum xs
runOp GtOp [x1, x2] = if x1 > x2 then 1 else 0
runOp LtOp [x1, x2] = if x1 < x2 then 1 else 0
runOp EqOp [x1, x2] = if x1 == x2 then 1 else 0
runOp op xs = error $ "wrong argument count to " ++ show op ++ ": " ++ show xs

printExpr :: Expr -> String
printExpr (Literal x) = show x
printExpr (OperationExpr op xs) =
  show op
    ++ "(\n"
    ++ indent (intercalate ",\n" (fmap printExpr xs))
    ++ "\n)"
  where
    indent = init . unlines . fmap ("  " ++) . lines

-- $> (>>= putStrLn . printExpr . expr) $ parseInputOrFail "9C0141080250320F1802104A08"

{- cases to toy around with, copied from the exercise
    C200B40A82 finds the sum of 1 and 2, resulting in the value 3.
    04005AC33890 finds the product of 6 and 9, resulting in the value 54.
    880086C3E88112 finds the minimum of 7, 8, and 9, resulting in the value 7.
    CE00C43D881120 finds the maximum of 7, 8, and 9, resulting in the value 9.
    D8005AC2A8F0 produces 1, because 5 is less than 15.
    F600BC2D8F produces 0, because 5 is not greater than 15.
    9C005AC2F8F0 produces 0, because 5 is not equal to 15.
    9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2.
-}

--- parsing

_tryInput :: Show a => Parser a -> String -> IO ()
_tryInput parser s = maybe (return ()) (parseTest parser) $ expandHex s

-- $> _tryInput packetP "D2FE28"

-- $> _tryInput packetP "38006F45291200"

-- $> _tryInput packetP "EE00D40C823060"

-- works on expanded binary form
binInputP :: Parser String
binInputP = do
  maybeBin <- expandHex <$> many1 (oneOf hexDigits)
  maybe (parserFail "Hex expansion failed??") return maybeBin

packetP :: Parser Token
packetP = do
  meta <- packetHeaderP

  case getType meta of
    4 -> LiteralP meta <$> literalPayloadP
    typeId -> OperatorP meta typeId <$> operatorPayloadP

operatorPayloadP :: Parser [Token]
operatorPayloadP = do
  lengthTypeId <- binDigitP
  if lengthTypeId == '0'
    then operatorPayload0P
    else operatorPayload1P

operatorPayload0P :: Parser [Token]
operatorPayload0P = do
  payloadLength <- intLenP 15
  payload <- count payloadLength binDigitP
  subParseP (many packetP) payload

operatorPayload1P :: Parser [Token]
operatorPayload1P = do
  payloadLength <- intLenP 11
  count payloadLength packetP

-- this is silly
subParseP :: Parser a -> String -> Parser a
subParseP parser s = case parse parser "subparse" s of
  Right x -> return x
  Left err -> parserFail $ "subparser failed with " ++ show err

literalPayloadP :: Parser Int
literalPayloadP = do
  leadingGroups <- many $ groupWithLeaderP '1'
  finalGroup <- option [] $ groupWithLeaderP '0'
  return $ bin2int $ concat leadingGroups ++ finalGroup
  where
    groupWithLeaderP :: Char -> Parser [Char]
    groupWithLeaderP c = char c *> count 4 binDigitP

packetHeaderP :: Parser PacketMeta
packetHeaderP = do
  version <- intLenP 3
  typeId <- intLenP 3
  return $ PacketMeta {getVersion = version, getType = typeId}

binDigitP :: Parser Char
binDigitP = oneOf "01"

intLenP :: Int -> Parser Int
intLenP n = bin2int <$> count n binDigitP

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
