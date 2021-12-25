{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module D24.Solution (solve) where

import Control.Monad.Coroutine.SuspensionFunctors (await)
import Control.Monad.State.Strict (State, get, gets, modify)
import Control.Monad.Trans (lift)
import D24.Coroutine (CoroutineVariadic, SuspendedProgram, Variadic, coroutineVariadic, feed, receive)
import Data.Bifunctor (first)
import Data.List (find, foldl')
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

-- $> :m + System.IO.Unsafe D24.Coroutine

-- $> testInput = unsafePerformIO $ readFile "inputs/d24-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  print $ solve1 input
  print $ solve2 input

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

_compile :: [Instruction] -> CoroutineVariadic ProgramState Int ProgramState
_compile ins =
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

solve1 :: Input -> Maybe Int
solve1 ins = fst <$> find snd (allMONADCalls ins)

--- part 2

solve2 :: a -> ()
solve2 = const ()

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
