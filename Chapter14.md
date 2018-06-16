# Chapter 14: Curry's Lively Bird Forest

## Problems

### Problem 1

First, I want to convert the constraints into predicate logic, so that
I can understand exactly what's being said. Let's work through laws
1-3:

```
y -> P AND ~x -> P AND (x AND P -> y)
(~y OR P) AND (x OR P) AND (~x OR ~P OR y)
(P OR (~y AND x)) AND ((~x OR y OR ~P)
(~(y OR ~x) OR P) AND (~P OR (y OR ~x))
(~x OR y) -> P AND P -> (~x OR y)
```

i.e. `P x y = ~x OR y`

(Finally.)

Law 4 says for all `x`, there exists `y` such that `y = P y x = ~y OR
x`. The only way to make this true is if `x` is always true.

We have a p = ~p or x. Only true if x is always true.

### Problem 2

To recreate Law 4, we want to find a combinator `y` s.t. `y = P y x`:

We can start with `(L P) (L P) = P ((L P) (L P))`, which is close but
not quite. Let's try:

```
(L (C P x)) (L (C P x))
  = C P x ((L (C P x)) (L (C P x)))
  = P ((L (C P x)) (L (C P x))) x
```

That does what we need, we've found the `y` for the given `x`.

Hooray.

Can we do this with just `C` or just `L`? I don't think so. `C` can't
build a fixed point as it lacks repetition. `L` on its own? I can't
prove it, but I believe it impossible.

### Problem 3

I thought as follows:

```
L (C P x) = B L (C P) x

((L (C P x)) (L (C P x)) = M (B L (C P) x) = B M (B L (C P)) x
```

However, `P` may not be proper! A combinator that, given `P` and `x`
produces `L (C P x)` would suffice:

```
Z x y z = L (C x y) z
        = C x y (z z)
        = x (z z) y
```

So, a combinator `Z` would do what we need.

## Bonus Exercises

### Exercise 1

a) `P x x = x OR ~x = TRUE`

b) `P y (P y x) = ~y OR ~y OR x = ~y OR x = P y x`

c) `P x y AND P y z = (~x OR y) AND (~y OR z) = ~x OR z = P x z`

d) `P x (P y z) = ~x OR ~y OR z = (~x AND y) OR (~y OR z)
   = ~(~y OR x) OR (~y OR z) = P (P y x) (P y z)`. I think there's a typo
   in the question in the book.

e) `P x (P y z)` = ~x OR ~y OR z = ~y OR ~x OR z = P y (P x z)`

Note that these exercises don't actually say it's "if and only if",
but it is.

### Exercise 2

`P y (P y x)` (given) and a) gives `P y x`.

`P y x` and `P (P y x) y` and b) gives `y`.

`y` and `P y x` and b) gives us `x`.

So, for all `x`, it's lively, and all birds in the forest are lively.

### Exercise 3

a) comes from 1b.

b) comes from Law 3.

c) `P y (P y x)` is `P y x`. `P (P y x) y` is `y`. Law 4 gives `P y x
= y`. Law 2 means at least one of them must be true, so they're both
true.
