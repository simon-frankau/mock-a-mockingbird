# Chapter 21: The Fixed Point Principle

## Problem 1

We want `A y = y A (A y A)`.

How about `A' a y = y a (a y a)` and feed it to `Y`?

First, let's construct `A'`:

```
A' a y = (y a) (a y a)
A' a   = S (C I a) (C a a)
       = (B S (C I) a) (W C a)
A'     = S (B S (C I)) (W C)
```

And check we haven't made a mistake:

```
S (B S (C I)) (W C) a y
 = B S (C I) a (W C a) y
 = S (C I a) (C a a) y
 = C I a y (C a a y)
 = I y a (a y a)
 = y a (a y a)
```

So, what we're after is `(B A' M) (B A' M)`. Let's call `A'' = (B A'
M)` and `A = A'' A''`:

```
A y = A'' A'' y
 = B (S (B S (C I)) (W C)) M A'' y
 = S (B S (C I)) (W C) (M A'') y
 = S (B S (C I)) (W C) A y
 = B S (C I) A (W C A) y
 = S (C I A) (C A A) y
 = C I A y (C A A y)
 = I y A (A y A)
 = y A (A y A)
```

Woohoo! QED. My memory of how to construct recursive functions using a
fixed-point operator is not entirely gone!
