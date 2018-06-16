# Chapter 15: Russell's Forest

## Problem 1

```
a' a' = ~(a a') = ~(a a)
```

Contradiction.

## Problem 2

```
Theta N = N (Theta N) = ~(Theta N)
```

Contradiction.

## Problem 3

At first I thought this might not lead to a contradiction, but I
peeked ahead and it did (but I didn't read how), and worked it out. I
should have worked it out without the hint.

```
Theta (A A) = A A (Theta (A A)) = ~(A OR Theta (A A))
```

Which implies `A`. So, `A A A` is false.

```
Theta (A (A A A)) = A (A A A) (Theta (A (A A A)))
  = ~(FALSE OR Theta (A (A A A)) = ~(Theta (A (A A A)))
```

Contradiction.
