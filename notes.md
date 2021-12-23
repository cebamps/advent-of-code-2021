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

*Note added the next day:* it turns out I could just use
`Control.Comonad.Store.Pointer` from the package `comonad-extras`. This is
almost exactly what I was longing for, based on Data.Array. I also just
discovered about 2D-indexed arrays with the `Ix` instance for tuples. I suppose
I'll be making use of that at some point in the future!

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

## Extra notes on comonads

Might as well jot them down here.

- `Control.Comonad.Store` very much reminds me of Conal Elliott's talk on
  denotational design applied to a graphics library:
  https://www.youtube.com/watch?v=bmKYiUOEo2A
- `experiment` in `ComonadStore` works on any functor, including those that
  would not be discrete collections. The functor could be a `(->) b` for example:
  `experiment :: (s -> (b -> s)) -> w a -> b -> a`. In an array, b could be an
  enum of directions (Up, Down, etc.) for example.


# Day 15

*Update:* Day 23 provided me a chance to reimplement Dijkstra using a more
efficient edge-tracking algorithm. I made it as a separate module and reused it
on D15 to check its correctness. It is much simpler and much more satisfying
now! Not to mention much more efficient as well: part 2 is solved in a little
over one second, compared to the previous 30 minutes runtime.

First one I'm very dissatisfied with! Step two ran very slowly, (around 30
minutes). I tried profiling, and it *seems* to indicate that the find-minimum
step of Dijkstra is to blame for the slowness, but... I'll bet the array data
structure I use to store the work in progress paths is to blame. Either because
of copying, or because of laziness? I don't think it's the laziness, because
the way I update it with accum is clearly documented to be strict in the
application of the accumulation function.

Here's the relevant excerpt from a profile run toward the beginning of part 2:

```
	   d15 +RTS -p -RTS

	total time  =        6.84 secs   (6844 ticks @ 1000 us, 1 processor)
	total alloc = 7,732,470,936 bytes  (excludes profiling overheads)

COST CENTRE        MODULE       SRC                                  %time %alloc

new-current        D15.Solution src/D15/Solution.hs:59:45-122         91.0   89.1
updateDistances    D15.Solution src/D15/Solution.hs:(64,1)-(73,51)     4.7    8.9
expand.\           D15.Solution src/D15/Solution.hs:59:70-98           3.6    0.0
tileRisk.newAssocs D15.Solution src/D15/Solution.hs:(106,5)-(112,7)    0.3    1.1


                                                                                                                          individual      inherited
COST CENTRE                               MODULE                  SRC                                  no.     entries  %time %alloc   %time %alloc

MAIN                                      MAIN                    <built-in>                           227           0    0.0    0.0   100.0  100.0
...
       expand                             D15.Solution            src/D15/Solution.hs:(56,1)-(61,97)   485         345    0.0    0.0    99.3   98.0
        expand.distances'                 D15.Solution            src/D15/Solution.hs:57:7-95          492         345    0.0    0.0     4.7    8.9
         new-distances                    D15.Solution            src/D15/Solution.hs:57:48-95         493         345    0.0    0.0     4.7    8.9
          currentEdge                     D15.Solution            src/D15/Solution.hs:45:5-15          494         345    0.0    0.0     0.0    0.0
          updateDistances                 D15.Solution            src/D15/Solution.hs:(64,1)-(73,51)   495         345    4.7    8.9     4.7    8.9
           updateDistances.distanceUpdate D15.Solution            src/D15/Solution.hs:69:5-71          502         698    0.0    0.0     0.0    0.0
            distances                     D15.Solution            src/D15/Solution.hs:47:5-13          503         339    0.0    0.0     0.0    0.0
           updateDistances.updateElem     D15.Solution            src/D15/Solution.hs:(71,5)-(73,51)   504         698    0.0    0.0     0.0    0.0
           distances                      D15.Solution            src/D15/Solution.hs:47:5-13          496         345    0.0    0.0     0.0    0.0
           updateDistances.nidx           D15.Solution            src/D15/Solution.hs:67:5-41          497         345    0.0    0.0     0.0    0.0
            updateDistances.keepIdx       D15.Solution            src/D15/Solution.hs:66:5-70          499        1380    0.0    0.0     0.0    0.0
             unvisited                    D15.Solution            src/D15/Solution.hs:46:5-13          501        1336    0.0    0.0     0.0    0.0
            neighbors                     D15.Solution            src/D15/Solution.hs:85:1-65          498         345    0.0    0.0     0.0    0.0
           updateDistances.keepIdx        D15.Solution            src/D15/Solution.hs:66:5-70          500           0    0.0    0.0     0.0    0.0
        expand.unvisited'                 D15.Solution            src/D15/Solution.hs:58:7-93          486         345    0.0    0.0     0.0    0.0
         new-unvisited                    D15.Solution            src/D15/Solution.hs:58:48-93         487         345    0.0    0.0     0.0    0.0
          currentEdge                     D15.Solution            src/D15/Solution.hs:45:5-15          488         345    0.0    0.0     0.0    0.0
          unvisited                       D15.Solution            src/D15/Solution.hs:46:5-13          489         345    0.0    0.0     0.0    0.0
        new-current                       D15.Solution            src/D15/Solution.hs:59:45-122        490         345   91.0   89.1    94.6   89.1
         expand.\                         D15.Solution            src/D15/Solution.hs:59:70-98         491    86102443    3.6    0.0     3.6    0.0
         idxMin                           D15.Solution            src/D15/Solution.hs:(52,1)-(53,55)   505         345    0.0    0.0     0.0    0.0
```


# Day 16

I would have liked to spend a bit more time to explore the
[`data-fix`](https://hackage.haskell.org/package/data-fix/) package and mabye
[`recursion-schemes`](https://hackage.haskell.org/package/recursion-schemes/)
also, to deal with AST representations and recursions a little more elegantly.
The packets and their general approach to AST representation could help me
abstract away a few things:

 - Defining a common data type between packets and expressions.
 - Folding over the tree of packets annotated with version info.
 - Recursively computing the value of the expression.


# Day 17

Part 1 was fun, pen and paper was enough to get the solution in closed form.
And there would not have been much reuse in part 2 if I had implemented more
programmatically.

In fact, solving part 1 gave me all the material I needed to make part 2
finite.

Some analysis let me separate the problem of part 2 in two to avoid quadratic
effects when going from the test case to the actual input.

But as a result, I find my solution to part 2 very verbose yet not quite
readable.

I hit a few obstacles along the way. The most troubling one turned out to be in
the most fundamental piece of the solution, which is the computation of the
trajectory. When evolving a coordinate, I started with (0, v0), which is the
current position and velocity, and evolved that until the velocity dropped to
zero. But the last steps are `(x_final - 1, 1) -> (x_final, 0)`. My unfold
dropped the last point, so I considered it stopped at `x_final - 1`.

For some reason this gave me all the correct trajectories for the test input.

Of course, after seeing I had too many trajectories and figuring I would check
them with a purpose-built debug function, I saw that they were all fine. But
that helped me pinpoint the root cause. I don't know what I would have done if
I had too little trajectories :)

Another funny obstacle I had was that I needed to deduplicate the velocities. I
didn't think of it at first, but one initial velocity (vx,vy) can hit the area
multiple times.


# Day 18

I spent a while reading things about zippers and trees.

I reckon trees with no values on internal nodes are relatively uncommon. I saw
slides from Edward Kmett calling them "leafy trees" so let's call them that.

It seems that unlike "actual" binary trees, leafy trees are not
straightforwardly comonadic: if I have a leafy tree of `a`s, I can't have a
`duplicate` producing something interesting on each node because only the `a`
values get changed to leafy trees. With binary trees on the other hand,
internal nodes can also contain themselves.

After exloring for some time for general typeclasses that I might reuse or
define, I decided to just build my zipper data type and tree walking methods as
standalones.

I'm a bit sad though, because my `walkZip` function pretty much folds over a
tree of zippers, though I never get to reify that tree because I would have to
define a separate data type for binary trees with values on internal nodes.

I also had a brief look at lenses (in particular `Control.Zipper` in the
`zippers` package), but I would rather play with that on a smaller, less
recursive problem. The API looks nice though, for what little I understand of
it.

For the record, I considered an alternate linear data structure that would be
explicitly indexable, and would contain the numbers + some location information
(depth, mostly). This would let me easily find the neighbors of any given leaf.
I would need efficient insertion / deletion too, to apply the rules.
`Data.Sequence` would work, I suppose?

Some nice references were useful:

- https://cs.ioc.ee/~tarmo/tsem05/uustalu0812-slides.pdf
- https://wiki.haskell.org/Zipper
- https://stackoverflow.com/questions/25519767/how-to-make-a-binary-tree-zipper-an-instance-of-comonad
- https://stackoverflow.com/questions/25554062/zipper-comonads-generically
