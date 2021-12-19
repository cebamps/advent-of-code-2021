module D18.Utility
  ( unfoldId,
    whileJust,
  )
where

-- We've seen this before in D11, and I had even made a note in a commit that I
-- had found something like this in the `zippers` package, how ironic.
whileJust :: (a -> Maybe a) -> a -> a
whileJust f = last . unfoldId f

-- variant of unfold where the state is the value
unfoldId :: (a -> Maybe a) -> a -> [a]
unfoldId f a = a : maybe [] (unfoldId f) (f a)
