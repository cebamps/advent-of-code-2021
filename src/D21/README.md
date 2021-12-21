A good way to reason about the problem is to see it as a breadth-first graph
walk. A player's turn takes them from one node (their position and total score)
to 7 others (moving forward by 3 to 9 steps), up until the end where instead
they jump into a "win" node. We accumulate an exponential number of paths, but
we don't have to track them independently. Instead we allow them to regroup by
assigning them an integer weight, to tame the combinatorial explosion.

A couple of notes:

 - The graph is directed (by definition) and acyclic (because score is strictly
   increasing).
 - There is a case to be made for memoization because some nodes may probably
   be reached at different times through different paths, but I won't bother
   implementing that here.

We can evolve the two players independently and in alternation. Assume no one
has won yet -- there is no postselection yet at that stage, and the states of
the players are truly independent. Now if at one round some of the 2-player
states reach a stopping condition (i.e., the player won), we can separate the
states where this has happened from the one where it has not. But since this
separation only involves looking at the state of the player who just played,
the total state remains in product form. Indeed, schematically, assuming p2
just played and before we apply postselection:

```
  P(p1 state & p2 state)
    =   P(p1 state) * P(p2 state & p2 still playing)
      + P(p1 state) * P(p2 state & p2 has won)
```

where P measures probability on the global space of events.

Instead of probabilities we will track the number of occurrences of each
event across all universes, be it locally (per player: `N_1(event)` and
`N_2(event)`) or globally (`N(event)`):

```
  N(p1 state & p2 state)
    =   N_1(p1 state) * N_2(p2 state & p2 still playing)
      + N_1(p1 state) * N_2(p2 state & p2 has won)
```

When we have encountered the winning condition, we can add the number of wins
(`N_2(p2 has won)`) to a counter and postselect on loss to continue tracking win
events in later rounds:

```
  N'(p1 state & p2 state)
   := N(p1 state & p2 state | p2 still playing)
    = N_1(p1 state) * N_2(p2 state | p2 still playing)
   := N'_1(p1 state) * N'_2(p2 state)
```

As we can see, since the postselection only applies to one player, we keep a
product structure.

As an example, consider a simplified game without positions: players roll a 1
or a 2, earn that as a score, and the game stops when one reaches 5. Without a
stopping condition, the players' scores evolve as such over 5 rounds: (where
(`k`) indicates `k` universes with that score -- `k` is the value of the `N_i`
function above for the given state of player i)

```
0 (1)
1 (1) 2 (1)
2 (1) 3 (2) 4  (1)
3 (1) 4 (3) 5  (3) 6  (1)
4 (1) 5 (4) 6  (6) 7  (4)  8 (1)
5 (1) 6 (5) 7 (10) 8 (10)  9 (5) 10 (1)
```

(this is Pascal's triangle, by the way)

What can we say happens after Player 1 has played round 3? There are 8 player 1
universes (fourth line of our Pascal triangle) and 4 player 2 universes (third
line), so 32 universes total in the product. In 4\*4 universes Player 1 has
reached score 5 or above and won, while in the remaining ones they haven't.
Hence, we will write down 16 wins for player 1 and resume the game with these
postselected universe counts, amounting to our 16 remaining universes:

```
p1: 3 (1) 4 (3)
p2: 2 (1) 3 (2) 4 (1)
```

After Player 2's turn, we have 32 universes laid out as such:

```
p1: 3 (1) 4 (3)
p2: 3 (1) 4 (3) 5 (3) 6  (1)
```

This makes 16 universes where Player 2 wins. And so on, until we run out of
universes on either side -- which we do, fortunately, because the score is
strictly increasing over time.

Back to our game now.

Let's consider the branching on a given round. This involves trinomial
coefficients: symbolically, after n rolls, the position of a player would be
transformed as `(J + J^2 + J^3)^n`, where `J` jumps one position ahead.

For three rolls, we have

```
(x + y + z)^3 = xxx + yyy + zzz + 3 (xyy + xzz + xxy + xzz + yyz + yzz) + 6 xyz

(J + J^2 + J^3)^3 = J^3 + J^6 + J^9 + 3 (J^5 + J^7 + J^4 + J^5 + J^7 + J^8) + 6 J^6
                  = J^3 + 3 J^4 + 6 J^5 + 7 J^6 + 6 J^7 + 3 J^8 + J^9
```

In other words, there is one universe in which we move 3 steps ahead, three
where we move 4 steps, six where we move 5, etc. This defines how we walk our
graph.

All that's left is assembling this into code.
