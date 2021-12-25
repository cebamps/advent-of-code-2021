# Day 24 solution

As stated in my main notes file, brute force was not going to work here. I had
to be more subtle about it, and the subtlety ended up extending right into a
solution, with very little computational help.


## Manual compilation

Inspecting the MONAD computation, we see that it is performed in 14 steps,
parameterized by 3 numbers, `a` `b` and `c`, with a single running variable `z`
(the register Z of the state). The other two registers of the ALU are
effectively discarded at each step.

Moreover the value of `a` is always either 1 or 26.

Letting w be the input, the new value of `z` is found to be

```
if z % 26 + b == w then
  z / a
else
  (z / a) * 26 + w + c
```


## Solution search

I was tempted to work at it backwards to see if we can get the previous-to-last
value of z, assuming that the last value is 0. Let `z' = 0` be that last value
and `z` be the preceding value.
Then,
 - If the condition held, we have `z / a == 0`.
    - If `a` is 1, that means `z = 0` and the condition becomes `w = b`.
    - If `a` is 26, `z` can be anything between -25 and 25, with `w = z + b`.
 - If the condition did not hold, there are more complicated admissible values
   for `z`. This doesn't seem tractable.

By working backwards we can't tell which less-significant digit choice is going
to lead to high more-significant digits at later (working backward still)
steps, so the search space still looks quite large.

Let's consider it forward again, with `z` being the starting value and `z'` the
next step value again. My input starts with (a,b,c) = (1,14,12), and `z = 0`,
so `z'` is defined by
```
if 14 == w then -- not happening
  0
else
  w + 12
```
we have `z' = w1 + 12` where `w1` denotes the first input.

Next, (1,15,7):
```
if (w1 + 12) % 26 + 15 == w2 then
  w1 + 12
else
  (w1 + 12) * 26 + w2 + 7
```

Again, quite hard to track.

I had to look for a little hint online to start noticing some patterns:
 - `z` must remain positive, as the only negative values are in `b`.
 - The condition `z % 26 + b == w` may therefore only hold true if `b < 10`.
 - In my input, every time `a == 1`, we also have `b >= 10` so we will always
   take the second branch.
 - `c` ranges from 1 to 15, so `c + w` is never higher than 25.
 - When `a == 26`, `b` ranges between -16 and 0 in my input, so the condition
   is `z % 26 == w + (-b)`.

This simplifies the step function:
```
if a == 1
  z * 26 + w + c
else if z % 26 == w + (-b)
  z / 26
else
  (z / 26) * 26 + w + c
```

And here is where it starts clicking :)


### Disentangling the steps

Consider the condition `z' % 26 == w' + (-b')`. If at the previous step `a ==
1` or the condition did not hold, we find that `z' % 26 == (w + c) % 26 == w
+ c`, so the condition to enter the middle branch is `w + c == w' - b'`.

Otherwise, z is the result of dividing the previous z by 26, so we will instead
find w and c values from earlier.

Intuitively, I get the picture that z grows under most steps, except when `a ==
26` and the inputs encode a rather specific sequence that can be derived from
the steps' `c` and `b` values. Hence I suppose the correct input for part 1
will start by picking the highest possible digits making z grow, but then go
into a more constrained sequence to go down the (z / 26) branch in time.

Since we multiply and divide the running `z` by 26 and only add `w + c` to it,
which is less than 26, can we maybe consider this running `z` as a stack of
base-26 digits? This would translate the function to a stack machine.

 - Multiplying by 26 is the same as inserting 0 at the bottom of the stack 
   `z = z * 26 + x` is `push x`. Note also that we never push 0.
 - Truncated division by 26 is the same as popping the bottom of the stack 
   `z = z / 26` is `pop`.
 - The remainder of the division by 26 is the value at the bottom of the stack 
   `z % 26` is `peek`.

```
if a == 1
  push (w + c)
else if peek == w + (-b)
  pop
else
  pop
  push (w + c)
```

That seems much more scrutable!


### Closed form solution for all accepted inputs

Now, in my input, the sequence of a == 1 is `T T T T F T T F F F T F F F`, and
each T is guaranteed to grow the stack. My goal is to get back to an empty
stack at the end, representing the final 0 value.

I will therefore need to shrink the stack at each F step, because there are as
many F as T steps. This means I must satisfy the condition `(peek == w + (-b)`
every time `a /= 1`. Then the stack depth will evolve as
`[1,2,3,4,3,4,5,4,3,2,3,2,1,0]`.

Assuming we satisfy the condition at every step, we can now actually reverse
the stack program!
```
if a == 1
  assert peek == w + c
  pop
else
  push w + (-b)
```

We can then take our steps and collapse them into a list of constraints. Going
back to a 1-indexed numbering:
```
step    1  2  3  4  5  6  7  8  9 10 11 12 13 14
a == 1  T  T  T  T  F  T  T  F  F  F  T  F  F  F
size    1  2  3  4  3  4  5  4  3  2  3  2  1  0
```

A constraint is formed by removing a pair of an F followed by a T. Here are the
constraints:
```
(11,12)
(7,8)
(6,9)
(4,5)
(3,10)
(2,13)
(1,14)
```

For each pair `(i,j)`, the constraint assert that `w_j - b_j == w_i + c_i`.
This gives us the full set of allowed inputs.


### Optimal solutions

#### Part 1

Since `i < j`, `w_i` is more significant so we'll pick the highest value of
`w_i` allowed such that `w_i` and `w_j` are between 1 and 9:

```
w_j = w_i + c_i + b_j

-> w_i = min(9, 9 - c_i - b_j)
   w_j = w_i + c_i + b_j
```

Thats' 12996997829399!

#### Part 2

On part 2, we need the smallest value of `w_i` instead:
```
w_j = w_i + c_i + b_j

-> w_i = max(1, 1 - c_i - b_j)
   w_j = w_i + c_i + b_j
```

That's 11841231117189!
