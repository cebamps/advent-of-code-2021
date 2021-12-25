{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module D24.Coroutine where

import Control.Monad.Coroutine (Coroutine, resume)
import Control.Monad.Coroutine.SuspensionFunctors (Await (Await))
import Control.Monad.State.Strict (State, runState)

-- no: we have no state here
-- newtype Vararg a b s = Vararg (Coroutine (Await a) (State s) b)

type SuspendedProgram s a b = Coroutine (Await a) (State s) b

-- encapsulates a stateful computation in progress
data Vararg a b
  = Vararg {runVararg :: a -> Vararg a b}
  | VarargDone b

feed :: a -> Vararg a b -> Vararg a b
feed x (Vararg f) = f x
feed _ (VarargDone _) = error "vararg takes no more arguments"

receive :: Vararg a b -> b
receive (VarargDone x) = x
receive (Vararg _) = error "vararg is not finished"

finish :: [a] -> Vararg a b -> b
finish [] vf = receive vf
finish (x : xs) vf = let vf' = feed x vf in finish xs vf'

execVararg :: s -> SuspendedProgram s a b -> Vararg a b
execVararg s p = do
  -- First, resume (peels Coroutine, get State (Either Suspended x)).
  -- Then, execute State with pending state value (peels State).
  -- Finally, depending on Coroutine progress, stop or continue, carrying the
  -- state along.
  case runState (resume p) s of
    (Left (Await f), s') -> Vararg $ \x -> execVararg s' (f x)
    (Right x, _) -> VarargDone x


{- ORMOLU_DISABLE -}
-- $> :m + Control.Monad.Trans Control.Monad.State Control.Monad.Coroutine.SuspensionFunctors
{- $>
 testRoutine :: Coroutine (Await Char) (State String) String
 testRoutine = do
   greeting <- lift get
   x <- trace "\nFirst await happened" <$> await
   y <- await
   z <- await
   return $ greeting ++ ", " ++ [x, y, z]
<$ -}
{- $>
 let partial = feed 'b' . feed 'a' $ execVararg "hello" testRoutine
  in (receive . feed 'c' $ partial, receive . feed 'd' $ partial)
<$ -}
{- ORMOLU_ENABLE -}
