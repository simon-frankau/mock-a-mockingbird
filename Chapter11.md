# Chapter 11: Birds Galore

## Bluebirds

### Problem 1

This is because `B` is the composition combinator - `B x y = x . y`.

### Problem 2

```
M (x . M) = M (B x M)
```

### Problem 3

From Problem 2 in Chapter 9, `(M . M) (M . M)` is its own fixed point.

```
(M . M) (M . M) = (B M M) (B M M)
```

### Problem 4

```
M (K . M) = M (B K M)
```

## Some Derivatives of the Bluebird

### Problem 5

Let's try a unification-based approach

```
            D x y z w = x y (z w)
D = B a     (B a) x y z w = x y (z w)
            a (x y) z w = x y (z w)
a = B       B (x y) z w = x y (z w)
            (x y) (z w) = (x y) (z w)
```

So, `D = B B`.

### Problem 6

This time, let's insert a pile of compositions:

```
B1 x y z w = x (y z w)
           = x . (y z) w
           = B x (y z) w
           = (B x) (y z) w
           = ((B x) . y) z w
           = B (B x) y z w
           = (B . B) x y z w
           = B B B x y z w

B1 = B B B
```

### Problem 7

```
E x y z w v = x y (z w v)
            = B (x y) (z w) v
            = B (B (x y)) z w v
            = B B B (x y) z w v
            = B (B B B) x y z w v
            = B B1 x y z w v

E = B B1
```

### Problem 8

```
B2 x y z w v = x (y z w v)
             = B x (y z w) v
             = B (B x) (y z) w v
             = B (B (B x)) y z w v
             = B (B B B x) y z w v
             = B B (B B B) x y z w v
             = D B1 x y z w v

B2 = D B1
```

### Problem 9

```
D1 x y z w v = x y z (w v)
             = B (x y z) w v
             = B B (x y) z w v
             = B (B B) x y z w v

D1 = B D
```

### Problem 10

```
B3 x y z w = x (y (z w))
           = B x y (z w)
           = B (B x y) z w
           = B B (B x) y z w
           = B (B B) B x y z w

B3 = B D B
```

### Problem 11

```
D2 x y z w v = x (y z) (w v)
             = B (x (y z)) w v
             = B (B x y z) w v
             = B B (B x y) z w v
             = B (B B) (B x) y z w v
             = B (B (B B)) B x y z w v

D2 = B (B D) B
```

### Problem 12

```
E' x y1 y2 y3 z1 z2 z3
    = x (y1 y2 y3) (z1 z2 z3)
    = B (x (y1 y2 y3)) (z1 z2) z3
    = B (B (x (y1 y2 y3))) z1 z2 z3
    = B (B B x (y1 y2 y3)) z1 z2 z3
    = B (B (B B x) (y1 y2) y3) z1 z2 z3
    = B B (B (B B x) (y1 y2)) y3 z1 z2 z3
    = B B (B (B (B B x)) y1 y2) y3 z1 z2 z3
    = B (B B) (B (B (B B x)) y1) y2 y3 z1 z2 z3
    = B (B (B B)) (B (B (B B x))) y1 y2 y3 z1 z2 z3
    = B (B (B B)) (B (B B (B B) x)) y1 y2 y3 z1 z2 z3
    = B (B (B B)) (B B (B B (B B)) x) y1 y2 y3 z1 z2 z3
    = B (B (B (B B))) (B B (B B (B B))) x y1 y2 y3 z1 z2 z3
    = B (B D1) (D (D D))

E' = B (B D1) (D (D D))
```

At this point, there's the question of whether all possible
"compositors" can be constructed from `B` - whether we can introduce
any bracketing that we like. We can. If you think of the expression as
a binary tree (applications as interior nodes), `B` introduces an
anticlockwise rotation. If we keep rotating the top-most node that has
a non-variable as its right child, we end up with an expression in
`B`s that introduces the brackets that we need. A key invariant is
that all the variables remain on the right-hand side of the tree, and
all the `B`s on the left, since we can't commute the order of leaf
nodes.

"Bluebird.hs" implements this algorithm, and generates some
alternative solutions to the problems in this chapter:

```
X (X X) -> B
X X (X X) -> B B
X (X X X) -> B B B
X X (X X X) -> B (B B B)
X (X X X X) -> B (B B B) B
X X X (X X) -> B (B B)
X (X (X X)) -> B (B B) B
X (X X) (X X) -> B B (B B)
X (X X X) (X X X) -> B (B B B) (B (B B B))
```

## Some Other Birds

### Problems 13-30

With the exceptions of the problems below, they're solved by
BirdsGalore.hs:

```
Problem 13: M = W (W K)
Problem 14: M = W I
Problem 15: I = W K
Problem 16: I = C K C
Problem 17: T = C I
Problem 20: R = B B T
Problem 21: C = R R R
Problem 21: C = B (T (B B T)) (B B T)
Problem 23: R = C C
Problem 24: F = B C R
Problem 25: F = E T T E T
Problem 26: F = B (T T) (B B (B B T))
Problem 27: V = C F
Problem 27: V = B (T (B (T T) (B B (B B T)))) (B B T)
Problem 28: V = R F R
Problem 29: F = C V
Problem 30: I = R R K
```

### Problem 18

Let's say all birds are fond of some bird x, so that for all y, y x =
x. T y x = x y, but (T y) is also fond of x, so x y = x = y x. x
commutes with everything.

...

Then it turns out I misinterpreted the question. They're not all fond
of the same bird.

T is fond of a bird A: T A = A. But T A x = A x and T A x = x A, so A
x = x A. This bird is the bird we're looking for. D'oh.

### Problem 19

The fixed point of T is `M (T . M) = M (B T M)`

### Problem 22

```
C x = R R R x = R x R
    = B B T x R = B (T x) R
```

## Some Relatives

Solved by BirdsGalore.hs:

```
Problem 31: X1 X2 X4 X3 = B C
Problem 32: X1 X3 X4 X2 = B C (B C)
Problem 33: X1 X4 X3 X2 = B C (B (B C) C)
Problem 34: X1 X4 X2 X3 = B (B C) C
Problem 35: X1 X2 X3 X5 X4 = B (B C)
Problem 35: X1 X2 X4 X5 X3 = B (B C (B C))
Problem 35: X1 X2 X5 X4 X3 = B (B C (B (B C) C))
Problem 35: X1 X2 X5 X3 X4 = B (B (B C) C)
```
