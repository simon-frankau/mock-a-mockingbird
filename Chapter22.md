# Chapter 22: Glimpse into Infinity

## Some Facts About Kestrels

### Problem 1

Let's say the forest contains at least two birds. If `I` is one of
them, let's make `J` the bird that isn't `I`. Then if `K` is fond of
`I`, `K I J = I J = J`, and `K I J = I`, so `I = J`, which is a
contradiction.

### Problem 2

Again, `J` is a bird that isn't `I`. If `K = I`, we have `K I J = I`
and `K I J = I I J = I J = J`, so `I = J`, and contradiction.

### Problem 3

For any `J` distinct from `K`:

```
S K = K
S K K = K K
S K K J = K K K
K J (K J) = K
J = K
```

Contradiction!

### Problem 4

`I` is fond of `K`, `S` is not, so `I` is not `S`.

### Problem 5

```
S = K
S K K = K K K
I = K
```

We've already proven `I` and `K` distinct.

### Problem 6

```
K x = K
K x y K = K y K
x K = y
```

This is true for all `y`, hence there's only one bird. Contradiction.

### Problem 7

```
K x = I
K x y = I y
x = y
```

Again, this is true for all `y`, hence contradiction.

## Some Nonegocentric Birds

### Problem 8

```
K = T
K K K = T K K
K = K K
```

Contradiction.

### Problem 9

```
T T = T
T T K (K K) = T K (K K)
K T (K K) = (K K) K
T = K
```

Contradiction.

### Problem 10

```
R I I = I
R I I (K K) = I (K K)
I (K K) I = K K
K K I = K K
K = K K
```

Contradiction.

Also:

```
R I = I
R I I = I I = I
```

And:

```
R = I
R I I = I I I = I I = I
```

### Problem 11

```
R R = R
R R I I = R I I
I I R = R I I
R = R I I
```

And contradiction from problem 10.

### Problem 12

```
C C = C
C C K I K = C K I K
C I K K = K K I
I K K = K K I
K K = K
```

Contradiction.

### Problem 13

```
V V = V
V V I (K K) I = V I (K K) I
(K K) V I I = I I (K K)
K I I = I I (K K)
I = K K
```

Contradiction.

(I don't think I'd previously proven that `I != K K`, but:

```
I = K K
I (K K) = K K (K K)
K K = K
```

### Problem 14

a.

```
W I = I
W I K = I K
I K K = K
K K = K
```

Contradiction.

b.

```
W W = W
W W K K = W K K
W K K K = K K K
K K K K = K
K K = K
```

Contradiction.

### Problem 15

a.

```
S I = I
S I I K = I I K
I K (I K) = I K
K (I K) = I K
K K = K
```

Contradiction.

Also `S = I` implies `S I = I I = I`, so `S` is not equal to `I`.

b.

```
S S = S
S S (K (K K)) K (K K) = S (K (K K)) K (K K)
S K ((K (K K)) K) (K K) = (K (K K)) (K K) (K (K K))
K (K K) ((K (K K)) K (K K)) = (K (K K)) (K K) (K (K K))
(K K) = (K K) (K (K K))
K K = K
```

Contradiction (very likely overengineered).

### Problem 16

a.

```
B K K = K K
B K K x y = K K x y
K (K x) y = K K x y
K x = K y
```

For all `x`, `y`, there's at least 2 birds, contradiction.

b.

```
B B = B
B B K I K K = B K I K K
B (K I) K K = K (I K) K
(K I) (K K) = K (I K) K
I = I K
I = K
```

Contradiction.

### Problem 17

Q x y z = y (x z)

```
Q Q = Q
Q Q (K K) (K I) K = Q (K K) (K I) K
(K K) (Q (K I)) = (K I) ((K K) K)
K = I
```

Contradiction.

## Kestrels and Infinity

### Problem 18

`K K K = K`

### Problem 19

I'll skip the hints and try to work it out for myself.

Let's call `K = K_0`, `K K = K_1`, `K (K K) = K_2` etc.

We have `K_n K = K_{n-1}` for all `n > 0`. So, if we're trying to
disprove `K_n = K_m` (wlog `n > m`), we just need to disprove `K_{n-m}
= K`.

To disprove `K_n = K` for `n > 2`, we can use induction, with `K_n K K
= K K K`, `K_{n-2} = K`, down to a base case of `K_1` or `K_2`. We
know `K != K K`, so we just need to prove `K != K (K K)`:

Assume

```
K = K (K K)
K (K K) K = K (K K) (K K) K
K K = (K K) K
K K = K
```

Contradiction.

## The Answers

Reading the answers in the book, I went round the houses sometimes,
but I think it worked out ok (I want to get through the book at this
stage, not optimise all answers!). I think I didn't really realise how
useful problem 6 would be for shortcutting the solutions of other
questions.

I found this chapter a bit disappointing for a "Glimpse Into
Infinity", and it feels really odd put in between meaty questions on
building recursive combinators and just before we dive into logic and
arithmetic. However, once again the book finally answers some
questions that have been nagging me for some time, effectively about
whether there are equivalent classes over the combinators that make
everything still work - the answer being "not really".
