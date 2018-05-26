# Chapter 9: To Mock a Mockingbird

## 1

"A is fond of B if AB = B" - More mathematically, B is a fixed point
of A.

Wow. Straight in with, effectively, "do all functions have a fixed
point?"! Harsh.

Let's use the standard fixed-point Y combinator trick, converted for
actual combinators rather than the lambda calculus:

```
X' = M (X . M)

X X' = X ((X . M) (X . M))

X' = (X . M) (X . M)
  = X (M (X . M))
  = X ((X . M) (X . M))

=> X X' = X'
```

## 2

Let's define X:

X = (M . M)

X X = (M . M) (M . M)
    = M (M (M . M))
    = (M (M . M)) (M (M . M))
    = ((M . M) (M . M)) ((M . M) (M . M))
    = (X X) (X X)

So, (X X) is its own fixed point.

## 3

Find the value of X where A agrees with B . A.

i.e.

A X = (B . A) X

Then

B (A X) = B ((B . A) X)
        = B (B (A x))

so A X is a fixed point of B.

Mockingbirds are agreeable with each bird being applied to themselves.

## 4
