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
