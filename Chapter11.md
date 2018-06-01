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
