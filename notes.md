## Day 2

Check this out:
https://github.com/mstksg/advent-of-code-2021/blob/master/reflections-out/day02.md

This implementation / explanation is lovely. It uses an outer semidirect
product group to combine aim and position into a monoid, and unifies both parts
under one monoidal abstraction. It's pretty neat! It's a perfect example of
laws being satisfied and nontrivial things falling out, such as the
associativity of the semigroup product which lets us combine actions as a
semigroup!

The article forgets to mention those laws though: it only works if the Action
instance respects the laws. But it's not too hard to prove them, i.e., that
`act m1` is an automorphism and that `act` is a homomorphism.

## Day 3

Part 2 is really just dealing with a prefix tree (trie).

My first approach did not see that, but it's pretty much what I was doing. I
transposed the input to turn it into what pretty much amounts to a list of
layers of my trie:

```
111      101      1  0
010  ->  111  ~=  1  1  (after sorting)
110      100      01 0
```

I redid it without implementing an actual trie, so it obscures the more
primitive folding operations.

## Day 5

### Project setup

I'm still poking around for a better project structure. The experience with
haskell-language-server and ghcid has been a little difficult when separating
the logic between lib and executable, because when editing the executable,
neither of them will account for changes in the library. This is addressed here
for HLS:
https://github.com/haskell/haskell-language-server/blob/1.5.1/docs/troubleshooting.md#problems-with-multi-component-tests-suites-executables-benchmarks-support-using-stack

So instead I put my logic in the lib and make the executable a rather dull
shell that feeds stdin input into the exported solve function.

Running ghcid also causes no trouble then: I can specify the Solution module
file by path.

For reference though, I found that it is possible to get more control over ghcid with this method:

```sh
ghcid -c 'stack ghci --no-load --ghci-options "-ghci-script ghci-whatever"' -a
```

where `ghci-whatever` is a file containing `:load src/Solution/D05.hs` for
example. Using a `.ghci` file allows to skip the `-ghci-script` argument
passing, but since there are multiple exercises I would require multiple ghci
script files.

### Parsing

Parsing-wise, I got to try my hand at parsec for the first time! I hesitated
between parsec, attoparsec and megaparsec. The thing that annoyed me is that I
saw no straightforward way to parse an integer number.

Now I understand better why that might be: the string representation of a
number is language-dependent. Even for unsigned decimal integers: do you allow
leading zeros? Thousands separators? If so, in what format? Do you allow "0x"
for hexadecimal parsing, or a "b" prefix to indicate binary? Signed or floating
point raise more questions, with scientific notation, spaces and so on.

For simplicity, I do a double parse: isolate the string representation of a
number with a parsec Parser (`many1 digit` for an unsigned int), and lift
the notoriously discouraged `read` into that parser.

### Solving

This one was much more pleasant! I think part of it is because I discarded
custom data types, which I find rather unwieldy for extraction and pattern
matching. And I didn't worry about using a Map to count instances of
coordinates either.


## Day 8

That one was tough! I did not want to brute force on all permutations, so I
went for a more efficient approach that builds the permutation progressively,
one scrambled digit at a time, and prunes paths that fail to unscramble the
next scramble digit into an available digit. It runs super quickly: about 70
milliseconds as timed by my shell.

I really liked how the recursion and branching could be easily dealt with by
leveraging laziness (to find my way around the tree of permutations until I met
the end) and the list monad (to abstract away the recursion and the branching;
it's really just flat-maps).

Debugging was a bit more difficult. I did it using `Debug.Trace` utilities to
watch the steps of my monadic fold. It would also have been a good opportunity
to try `ghci`'s debugging abilities.


## Day 9

I saw a 2D grid and convolution and figured it was the right time to try
comonads, which I had been intrigued by.

Very roughly speaking, in my own intuitive terms, a comonad is a functor that
allows you to "zoom out".

It is dual to monads in the sense that its operations are flipped: `duplicate`
is a backward `join` (in other word, turning your `m a` into an `m (m a)`), and
`extract` is a backward `return` `return` (in other words, turning your `m a`
into an `a`).

The "zooming out" happens because of comonad laws. With `duplicate` each value
is turned into a comonad value. If we think of functors as containers, a value
then becomes a container of values. In addition, that container must be
"focused" on the original value such that `extract` returns it:

```hs
fmap extract . duplicate = id
```

The `comonad` package provides `Store s a` in `Control.Comonad.Store`, which is
built from an accessor function of type `(s -> a)` and an initial focus of type
`s`. It provides facilities to move the focus around, which are built around
the primitives `pos` and `peek`:

```hs
-- get the focus point
pos :: Store s a -> s

-- extract a value at another focus point
peek :: s -> Store s a -> a

-- extract a value at another focus point relative from the current one
peeks :: (s -> s) -> Store s a -> a

-- move the focus point
seek :: s -> Store s a -> Store s a

-- move the focus point relative to the current one
seeks :: (s -> s) -> Store s a -> Store s a

-- I didn't think too much about that one but I should have: sample values
-- around the focus point
experiment :: Functor f => (s -> f s) -> Store s a -> f a
```

Combining those with `extend :: (Store s a -> b) -> Store s a -> Store s b`,
which is dual to the monadic bind `>>=`, lets us do convolutions! The argument
to extend can sample around the focus, and the final return value gives us the
samples in the same comonad structure.

Unfortunately for me, Store is quite general and works with an accessor lambda.
This makes it hard to read my convolution as a list -- all of its structure is
hidden so I can't iterate over the convolution for example.

I work around this by taking the indexes from the original list and sampling
the convolution with those.

I'm pretty sure a more specialized implementation of ComonadStore (the
typeclass that provides `pos`, `peek` and friends) would be possible for
structures such as a vector of vectors or a `Map (Int, Int) a` with
accompanying focus, though that instance has to be able to deal with empty
values.

I tried implementing `Comonad` for a focused version of `Map (Int, Int) (Maybe
a)` but got stuck on implementing `duplicate` on the Comonad instance. I guess
I've made that work now, I'll have to try that later.

```hs
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Foo where

import Control.Comonad
import Data.Map (Map, mapWithKey)

type Idx = (Int, Int)
data Field i a = Field { fFocus :: i, fValues :: Map i a } deriving Functor

instance Comonad (Field Idx) where
  -- untested. Should return a Field Idx (Field Idx a).
  -- Each value of the map is replaced by the entire map, with the focus moved
  -- onto it.
  duplicate Field{..} = Field {fFocus, fValues = mapWithKey (\k a -> Field k fValues) fValues}
```

Anyway, I went for the former approach explained above.

And now that I have an abstraction to walk over my map, in principle it is not
too difficult to do interesting things without explicit looping. I was afraid
with part 2, both because the comonad would be inefficient (basin computation
sounds like a good candidate for memoization) and complex to wield.

Thankfully, for the second part, the assumption that each basin has a single
lowest point and every point of the basin slopes down into it is a life saver:
I can identify the basin independently for each grid position. I don't know how
the comonad abstraction would hold up otherwise without becoming too complex
for my poor little mind to reason about.

And for the first part, surprisingly, it's super fast. Less than 48
milliseconds. I did not even need to switch out my grid representation from
nested lists to something more adapted to random access, like the `(Int, Int)`
map above or nested vectors.

Of course I am well aware that there are likely more straightforward solutions.
But once I had the comonad abstraction, it really did get out of the way and I
could focus on the rest! I'm really glad I could give it a spin after reading
about it here and there.
