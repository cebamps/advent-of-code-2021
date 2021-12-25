{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module D24.Solution (solve) where

import Control.Monad (forM_)
import Control.Monad.Coroutine.SuspensionFunctors (await)
import Control.Monad.State.Strict (State, get, gets, modify)
import Control.Monad.Trans (lift)
import D24.Coroutine (CoroutineVariadic (CoroutineVariadic), PureVariadic (PureVariadic, PureVariadicDone), SuspendedProgram, Variadic, coroutineVariadic, feed, finish, receive)
import Data.Bifunctor (first)
import Data.List (find, foldl', tails, sort)
import Data.Maybe (fromJust)
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe D24.Coroutine

-- $> testInput = unsafePerformIO $ readFile "inputs/d24-test.txt" >>= parseOrFail inputP

-- $> trueInput = unsafePerformIO $ readFile "inputs/d24.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  investigate input

  putStrLn "\nManual solution follows, check notes for details!"

  print $ solve1 input
  print $ solve2 input

  --putStrLn "\nBrute force search will run now, but it's a lost cause..."
  --print $ _solve1Bruteforce input

data Register = W | X | Y | Z deriving (Eq, Show)

data Instruction
  = Inp Register
  | Add Register (Either Register Int)
  | Mul Register (Either Register Int)
  | Div Register (Either Register Int)
  | Mod Register (Either Register Int)
  | Eql Register (Either Register Int)
  deriving (Eq, Show)

data ProgramState = ProgramState
  { pW :: Int,
    pX :: Int,
    pY :: Int,
    pZ :: Int
  }
  deriving (Eq, Show)

type Input = [Instruction]

--type SuspendedProgram a = Coroutine (Await Int) (State ProgramState) a

setReg' :: Register -> Int -> (ProgramState -> ProgramState)
setReg' W x st = st {pW = x}
setReg' X x st = st {pX = x}
setReg' Y x st = st {pY = x}
setReg' Z x st = st {pZ = x}

setReg :: Register -> Int -> State ProgramState ()
setReg r = modify . setReg' r

getReg' :: Register -> ProgramState -> Int
getReg' W = pW
getReg' X = pX
getReg' Y = pY
getReg' Z = pZ

getReg :: Register -> State ProgramState Int
getReg = gets . getReg'

deref :: Either Register Int -> State ProgramState Int
deref (Right x) = return x
deref (Left x) = getReg x

operate :: (Int -> Int) -> Register -> State ProgramState ()
operate f r = getReg r >>= setReg r . f

operate2 :: (Int -> Int -> Int) -> Register -> Either Register Int -> State ProgramState ()
operate2 f r x = do
  x' <- deref x
  operate (`f` x') r

runInstruction :: Instruction -> SuspendedProgram ProgramState Int ()
runInstruction (Inp r) = await >>= lift . setReg r
runInstruction (Add r x) = lift $ operate2 (+) r x
runInstruction (Mul r x) = lift $ operate2 (*) r x
runInstruction (Div r x) = lift $ operate2 quot r x
-- not clear in the instructions if it is the remainder of the previous
-- operation, or the mod operation
runInstruction (Mod r x) = lift $ operate2 rem r x
runInstruction (Eql r x) = lift $ operate2 eqFun r x
  where
    eqFun :: Int -> Int -> Int
    eqFun y y' = if y == y' then 1 else 0

initState :: ProgramState
initState = ProgramState {pW = 0, pX = 0, pY = 0, pZ = 0}

compile :: [Instruction] -> CoroutineVariadic ProgramState Int ProgramState
compile ins =
  let program = mapM_ runInstruction ins >> lift get
   in coroutineVariadic initState program

-- $> [ (w,x,y,z) | i <- [0..19], let (ProgramState w x y z) = finish [i] $ _compile testInput]

compileMONAD :: [Instruction] -> CoroutineVariadic ProgramState Int Bool
compileMONAD ins =
  let program = mapM_ runInstruction ins >> lift ((0 ==) <$> getReg Z)
   in coroutineVariadic initState program

--- part 1

digitsToInt :: [Int] -> Int
digitsToInt = foldl' shift 0
  where
    shift s x = s * 10 + x

allMONADCalls :: [Instruction] -> [(Int, Bool)]
allMONADCalls ins = first digitsToInt <$> allBranches 14 (compileMONAD ins)

-- first inputs are listed first in the output
allBranches :: Variadic f => Int -> f Int a -> [([Int], a)]
allBranches = go
  where
    go :: Variadic f => Int -> f Int b -> [([Int], b)]
    go 0 vf = [([], receive vf)]
    go n vf =
      concatMap
        ( \i ->
            [ (i : input, out)
              | let vf' = feed i vf,
                (input, out) <- go (n - 1) vf'
            ]
        )
        digits
    digits = [9, 8 .. 1]

handCompilationParameters :: [Instruction] -> Maybe (Int, Int, Int)
handCompilationParameters
  [ Inp W,
    Mul X (Right 0),
    Add X (Left Z),
    Mod X (Right 26),
    Div Z (Right zdiv5),
    Add X (Right xadd6),
    Eql X (Left W),
    Eql X (Right 0),
    Mul Y (Right 0),
    Add Y (Right 25),
    Mul Y (Left X),
    Add Y (Right 1),
    Mul Z (Left Y),
    Mul Y (Right 0),
    Add Y (Left W),
    Add Y (Right yadd16),
    Mul Y (Left X),
    Add Z (Left Y)
    ] = Just (zdiv5, xadd6, yadd16)
handCompilationParameters _ = Nothing

handCompiledStep :: [Instruction] -> (Int -> Int -> Int)
handCompiledStep (handCompilationParameters -> Just (a, b, c)) = \inp z ->
  if inp == z `rem` 26 + b
    then z `quot` a
    else (z `quot` a) * 26 + inp + c
handCompiledStep _ = error "instructions do not follow the template"

blocks :: [Instruction] -> [[Instruction]]
blocks = partition 18

handCompiled :: [Instruction] -> PureVariadic Int Int
handCompiled ins =
  let steps = handCompiledStep <$> blocks ins
   in mkv 0 steps
  where
    mkv :: Int -> [Int -> Int -> Int] -> PureVariadic Int Int
    mkv z [] = PureVariadicDone z
    mkv z (step : steps) = PureVariadic $ \inp ->
      let z' = step inp z
       in mkv z' steps

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = let (h, t) = splitAt n xs in h : partition n t

checkHandCompiled :: [Instruction] -> [Int] -> Bool
checkHandCompiled ins inp =
  let vf = compile ins
      vf' = handCompiled ins
   in finish inp vf' == getReg' Z (finish inp vf)

investigate :: [Instruction] -> IO ()
investigate ins = do
  let vf = coroutineVariadic initState $ mapM_ runInstruction ins >> lift get
      vf' = foldr1 (.) (replicate 13 (feed 9)) vf
      CoroutineVariadic (_, stateInProgress) = vf'
  putStrLn "State after feeding 13 times 9:"
  print stateInProgress

  putStrLn "\nManual solve test:"
  let sol = [1, 2, 9, 9, 6, 9, 9, 7, 8, 2, 9, 3, 9, 9]
   in print $ finish sol vf

  putStrLn "\n9 branches:"
  forM_ [1 .. 9] $ \i -> do
    putStr $ show i ++ " -> "
    print $ receive . feed i $ vf'

  putStrLn "\nHand compilation: list of (a,b,c) parameters for each step:"
  forM_ (zip [1 :: Int ..] $ handCompilationParameters <$> blocks ins) $ \(i, Just params) -> do
    putStrLn $ show i ++ " -> " ++ show params

  putStrLn "\nComparison with hand-compiled version for a few inputs:"
  forM_ (take 9 . fmap (take 14) . tails $ cycle [1 .. 9]) $ \inp -> do
    putStr $ show inp ++ " -> "
    print $ checkHandCompiled ins inp

--- Discarding almost all of the above, here's my manual solution.

-- see Solution.md
constraintPairs :: [(Int, Int)]
constraintPairs = fmap (\(i, j) -> (i - 1, j - 1)) [(11, 12), (7, 8), (6, 9), (4, 5), (3, 10), (2, 13), (1, 14)]

-- unsafe
inputs1 :: [Instruction] -> [(Int, Int)]
inputs1 inp =
  let abcs = fromJust . handCompilationParameters <$> blocks inp
   in concat
        [ [(i, wi), (j, wj)]
          | (i, j) <- constraintPairs,
            let (_, _, ci) = abcs !! i,
            let (_, bj, _) = abcs !! j,
            let wi = min 9 (9 - ci - bj),
            let wj = wi + ci + bj
        ]

inputs2 :: [Instruction] -> [(Int, Int)]
inputs2 inp =
  let abcs = fromJust . handCompilationParameters <$> blocks inp
   in concat
        [ [(i, wi), (j, wj)]
          | (i, j) <- constraintPairs,
            let (_, _, ci) = abcs !! i,
            let (_, bj, _) = abcs !! j,
            let wi = max 1 (1 - ci - bj),
            let wj = wi + ci + bj
        ]

solve1 :: Input -> Int
solve1 = digitsToInt . fmap snd . sort . inputs1

-- lost cause
_solve1Bruteforce :: Input -> Maybe Int
_solve1Bruteforce ins = fst <$> find snd (allMONADCalls ins)

--- part 2

solve2 :: Input -> Int
solve2 = digitsToInt . fmap snd . sort . inputs2

--- parsing

inputP :: Parser Input
inputP = sepEndBy instructionP endOfLine <* eof

instructionP :: Parser Instruction
instructionP =
  choice . fmap try $
    [ Inp <$ string "inp" <*> (spaces *> regP),
      Add <$ string "add" <*> (spaces *> regP) <*> (spaces *> regOrLitP),
      Mul <$ string "mul" <*> (spaces *> regP) <*> (spaces *> regOrLitP),
      Div <$ string "div" <*> (spaces *> regP) <*> (spaces *> regOrLitP),
      Mod <$ string "mod" <*> (spaces *> regP) <*> (spaces *> regOrLitP),
      Eql <$ string "eql" <*> (spaces *> regP) <*> (spaces *> regOrLitP)
    ]

regP :: Parser Register
regP =
  W <$ char 'w'
    <|> X <$ char 'x'
    <|> Y <$ char 'y'
    <|> Z <$ char 'z'

regOrLitP :: Parser (Either Register Int)
regOrLitP = Left <$> regP <|> Right <$> intP
  where
    intP :: Parser Int
    intP = fmap read $ maybe id (:) <$> optionMaybe (char '-') <*> many1 digit

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
