{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module D24.Coroutine
  ( Variadic (feed, receive, finish),
    PureVariadic (PureVariadic, PureVariadicDone),

    CoroutineVariadic (CoroutineVariadic),
    SuspendedProgram,
    coroutineVariadic,
  )
where

import Control.Monad.Coroutine (Coroutine, resume)
import Control.Monad.Coroutine.SuspensionFunctors (Await (Await))
import Control.Monad.State.Strict (State, runState)

type SuspendedProgram s a b = Coroutine (Await a) (State s) b

class Variadic f where
  feed :: a -> f a b -> f a b
  receive :: f a b -> b
  finish :: [a] -> f a b -> b
  finish [] vf = receive vf
  finish (x : xs) vf = let vf' = feed x vf in finish xs vf'

data PureVariadic a b
  = PureVariadic (a -> PureVariadic a b)
  | PureVariadicDone b

instance Variadic PureVariadic where
  receive (PureVariadicDone x) = x
  receive _ = error "computation is not finished"
  feed x (PureVariadic f) = f x
  feed _ (PureVariadicDone _) = error "computation takes no more arguments"

-- encapsulates a stateful computation in progress
newtype CoroutineVariadic s a b
  = CoroutineVariadic
      ( -- contents of the state monad after resumption
        Either (Await a (SuspendedProgram s a b)) b,
        s
      )

-- First, resume (peels Coroutine, get State (Either Suspended x)).
-- Then, run State with pending state value (peels State).
-- Finally, depending on Coroutine progress, stop or continue, carrying the
-- state along.
coroutineVariadic :: s -> SuspendedProgram s a b -> CoroutineVariadic s a b
coroutineVariadic st sp = CoroutineVariadic $ runState (resume sp) st

instance Variadic (CoroutineVariadic s) where
  receive (CoroutineVariadic (Right x, _)) = x
  receive _ = error "computation is not finished"
  feed x (CoroutineVariadic (Left (Await aw), st)) = coroutineVariadic st (aw x)
  feed _ (CoroutineVariadic (Right _, _)) = error "computation takes no more arguments"

{- ORMOLU_DISABLE -}
-- $> :m + Control.Monad.Trans Control.Monad.State.Strict Control.Monad.Coroutine.SuspensionFunctors Debug.Trace
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
 let partial = feed 'b' . feed 'a' $ coroutineVariadic "hello" testRoutine
  in (receive . feed 'c' $ partial, receive . feed 'd' $ partial)
<$ -}
{- ORMOLU_ENABLE -}
