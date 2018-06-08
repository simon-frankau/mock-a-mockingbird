# Chapter 9: To Mock a Mockingbird

I found 1-3 really much harder than the rest of the questions in this
chapter.

I notice a bunch of my answers are a bit more constructive than those
given in the book, and may in places be a little invalid as they rely
on assumptions that are true in how we tend to actually play with
combinators (with looser definitions of equality, etc.), but aren't
allowed in this slightly formalised setting.

## To Mock a Mockingbird

### Problem 1

"A is fond of B if AB = B" - More mathematically, B is a fixed point
of A.

Wow. This questions is straight in with, effectively, "do all
functions have a fixed point?"! Harsh.

Let's use the standard Y combinator trick for finding a fixed point,
converted for actual combinators rather than the lambda calculus:

```
X' = M (X . M)

X X' = X ((X . M) (X . M))

X' = (X . M) (X . M)
  = X (M (X . M))
  = X ((X . M) (X . M))

=> X X' = X'
```

### Problem 2

Let's define X:

```
X = (M . M)

X X = (M . M) (M . M)
    = M (M (M . M))
    = (M (M . M)) (M (M . M))
    = ((M . M) (M . M)) ((M . M) (M . M))
    = (X X) (X X)
```

So, `(X X)` is its own fixed point.

(This is `Y M`.)

### Problem 3

Let's find the value `X` where `A` agrees with `B . A`.

i.e.

```
A X = (B . A) X
```

Then

```
B (A X) = B ((B . A) X)
        = B (B (A x))
```

so `A X` is a fixed point of `B`.

Mockingbirds are agreeable with each bird being applied to themselves.

This is a particularly funky result to me. I'm used to lambda calculus
and finding fixed points with combinators like the Y combinator. It's
pretty constructive. In comparison, here we're defining functions by
behaviour. The "agreeable bird" is a kind of diagonalisation
construction (like with Cantor's counting of the reals), and I find it
really surprising, since this suffices to create fixed points - you
don't need something as "simple" as M. I guess the way the fixed-point
behaviour is introduced is that you can find the point at which the
"agreeable bird" agrees with *itself* composed with something else.

### Problem 4

If C is agreeable, then for any bird D, there'll be a bird X s.t.
`C X = (D . B) X`. This means

```
(A . B) X = (D . B) X =
A (B X) = D (B X)
```

Setting `Y = B X~, then we have `A Y = D Y`, so A is agreeable too.

### Problem 5

Show there's a bird s.t. `D x = A (B (C x))`.

Composition means there's a bird `E = B . C`, `D x = A (E x)`, and
then `D = A . E`.

### Problem 6

Problem 1 means `(A . B)` has a fixed point `y`. Define `x = B y`, and
the result follows.

### Problem 7

`x = y =` the fixed point of the bird.

### Problem 8

If `A` is happy, `A . A` is normal.

## Hopeless Egocentricity

### Problem 9

"Kestrels" are the constants combinator K, that makes a constrant
function when given a value to always return.

We're after the fixed point of `K`, `Y K`. In the combinators we have,
this is `M (K . M)`.

```
M (K . M) x = (K . M) (K . M) x
            = K (M (K . M)) x
            = M (K . M)
```

### Problem 10

I read this as:

For all z, `x z = y`. Deos this mean `x a = y a` for some a?

`x a = y`, so this is only true if `y = y a` for some a - and it's
quite possible to create a bird that never returns itself.

However, that's not what the question asks. It asks "is it fond?" (`x
y = y`), not "is it agreeable"? D'oh. It's obviously fond, if it's
fixated.

D'oh.

### Problem 11

If `K K = K`, then `K K x = K x = K`.

### Problem 12

If `(K x) (K x) = (K x)` then for any y, `(K x) (K x) y = K x y`, so
`K x = x`.

### Problem 13

`A x = A = A y`

### Problem 14

`A x y = A y = A`

### Problem 15

`A x = A`, so `A x` is also hopelessly egocentric.

### Problem 16

If `K x = K y`, then `K x K = K y K`, so `x = y`.

### Problem 17

If A x = y for all x, and A x = z for all x, then x = z.

### Problem 18

If `K (K x) = K x`, then `K x = x` by cancellation law.

### Problem 19

If K is hopelessly egocentric, `K x K = K K = K`, but also `K x K =
x`, so `K = x`. `K` is the only bird in the forest.

## Identity birds

### Problem 20

The identity function is nice and straightforward. Phew.

For all B, there's an x s.t. `I x = B x`, i.e. `B x = x`, so B is fond
of `x`, and all birds are fond of some bird.

### Problem 21

Yes, it's agreeable - for each bird, it agrees on the bird that it is
fond of.

### Problem 22

1 and 2 are equivalent - if I agrees with B on a bird, B is fond of
that bird.

Say `B I = B` and `B B = I`. Then x and y can be I and B, they are
compatible, but B is not fond of any bird.

### Problem 23

`I x = x`. If hopelessly egocentric, `I x = I`, hence `x = I`, hence I
is the only bird in the forest.

## Larks

### Problem 24

`L I = M`

### Problem 25

We want to find a fixed point for all birds.

Working backwards, `L x y = x (y y) = x (M y) = (x . M) y`, so `L x =
x . M`. We want `M (x . M)`, but it's also ok to apply x to this,
since it's a fixed point - `x (M (x . M)) = (x . M) (x . M) = (L x)(L
x)`.

Let's check, by expand the left L: `(L x)(L x) = x ((L x)(L x))`. Yep,
looks good.

### Problem 26

A hopelessly egotistic lark can be used to generate the fixed point of
any bird that will also be a lark. So, every bird is fond of it.

### Problem 27

Why can't `L K = K`? `L K x y = K (x x) y = x x`, and `K x y = x`.

`x x = x` for all `x`.

This means K must be egocentric, and by 11 it's hopelessly egocentric,
and there's only 1 bird. This contradicts L and K being different.

### Problem 28

`L = K L x = L x`, set `x = B (L B)`, which is a fixed point of `B`. We have:

`K L (B (L B)) = L` and `K L (B (L B)) = L (B (L B)) = B ((L B) (L
B))`, so `L` is a fixed point of all birds.

### Problem 29

This is where I write code to brute-force the search, because it's
fun!

```
$ ./Combinator
1
2
3
4
5
6
7
8
9
10
(((L ((L (L L)) (L (L L)))) ((L L) L)),True)
11
12
((((L (L L)) (L (L L))) ((L (L L)) ((L L) L))),True)
((((L (L L)) ((L L) L)) ((L (L L)) (L (L L)))),True)
((((L (L L)) ((L L) L)) ((L (L L)) ((L L) L))),True)
```

The key result is really:

```
((L (L L)) (L (L L))) ((L (L L)) (L (L L)))
```

We can rewrite this by saying `M = L (L L)`, which makes this
expression `(M M) (M M)`. We find

```
M x = L (L L) x = L L (x x) = L ((x x) (x x))
```

Then we define `N = (M M) (M M)`:

```
N = (M M) (M M)
  = (L ((M M) (M M)) (M M)
  = ((M M) (M M)) ((M M) (M M))
  = N N
```

QED!
