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
