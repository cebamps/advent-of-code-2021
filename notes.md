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
