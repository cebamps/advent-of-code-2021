{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module D24.Coroutine where

import Control.Monad.Coroutine (Coroutine, Naught, foldRun, pogoStick, resume, runCoroutine)
import Control.Monad.Coroutine.SuspensionFunctors (Await (Await), await)

data Vararg a b
  = Vararg {runVararg :: a -> Vararg a b}
  | VarargDone b

feed :: a -> Vararg a b -> Vararg a b
feed x (Vararg f) = f x
feed _ (VarargDone _) = error "vararg takes no more arguments"

finish :: Vararg a b -> b
finish (VarargDone x) = x
finish (Vararg _) = error "vararg is not finished"

-- plan: don't compile into a [Int] -> ProgramState, compile into a generator
-- coroutine type of deal where each input is processed in turn

-- Concretely, i want a program to be a function that takes an input and
-- returns either a program or a final value.

-- Control.Monad.Coroutine
-- foldRun :: Monad m => (a -> s (Coroutine s m x) -> (a, Coroutine s m x)) -> a -> Coroutine s m x -> m (a, x)
-- specializing to a = [a], with s = Await Int, m = my internal state monad, Coroutine' = Coroutine s m
--
-- ([a] -> Await Int (Coroutine' x) -> ([a], Coroutine' x)) -> [a] -> Coroutine' x -> m ([a], x)
--
-- the first function will be
-- f :: [a] -> Await Int (Coroutine' x) -> ([a], Coroutine' x)
-- f [] _ = error "ran out of inputs"
-- f (x:xs) (Await continue) = (xs, continue x)
--
-- alternatively we could also weave yields into the awaits using the package's
-- machinery by defining a Coroutine (Yield Int) Identity () from the inputs:
--
-- inputGen inputs = mapM_ yield inputs
-- -- something like this, typechecker would help
-- myWeavedCoroutine = weave sequentialBinder weaveAwaitYield (inputGen inputs) myCoroutine

-- I don't know why this is not provided, but here it is.
runFinishedCoroutine :: (Functor s, Monad m) => Coroutine s m a -> m (Maybe a)
runFinishedCoroutine = pogoStick (const (return Nothing)) . fmap Just

runToCompletion :: Monad m => [a] -> Coroutine (Await a) m x -> m ([a], x)
runToCompletion = foldRun f
  where
    f [] _ = error "ran out of inputs"
    f (x : xs) (Await continue) = (xs, continue x)

asVararg :: Monad m => (forall y. m y -> y) -> Coroutine (Await a) m x -> Vararg a x
asVararg unwrap co = case unwrap (resume co) of
  Right x -> VarargDone x
  Left (Await cont) -> Vararg $ asVararg unwrap . cont

-- $> :m + Data.Functor.Identity Debug.Trace

{- $>
  testRoutine :: Coroutine (Await Char) Identity String
  testRoutine = do
    x <- traceShow "First await" <$> await
    y <- await
    z <- await
    return $ "hello, " ++ [x, y, z]
<$ -}
--
{- $>
  let partial = feed 'b' . feed 'a' $ asVararg runIdentity testRoutine
   in (finish . feed 'c' $ partial, finish . feed 'd' $ partial)
<$ -}

-- we will want to bounce the coroutine and then run runFinishedCoroutine on it
