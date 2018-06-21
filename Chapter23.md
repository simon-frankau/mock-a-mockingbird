# Chapter 23: Logical Birds

This chapter, I'm back to using `BirdsGalore.hs` again...

## My answers

### Problem 1

We want `N p t f = p f t`. So, `N = C`.

### Problem 2

We want `c p q t f = p (q t f) f`.

`Problem 2: X1 (X2 X3 X4) X4 = B B (B S C)`

### Problem 3

We want `d p q t f = p t (q t f)`.

`Problem 3: X1 X3 (X2 X3 X4) = B S (B B)`

### Problem 4

We want `i p q t f = p (q t f) t`

`Problem 4: X1 (X2 X3 X4) X3 = B S (B (B B) C)`

(Could just compose negation and disjunction - `B d N`)

### Problem 5

We want `e p q t f = p (q t f) (q f t)`

`BirdsGalore.hs`'s search didn't do so well, so I had a go manually:

```
e p q t f
 = p (q t f) (q f t)
 = p (q t f) ((C q) t f)
 = B p (q t) f ((C q) t f)
 = S (B p (q t)) ((C q) t) f
 = S (B (B p) q t) ((C q) t) f
 = B S (B (B p) q) t ((C q) t) f
 = S (B S (B (B p) q)) (C q) t f
 = S (B (B S) (B (B p)) q) (C q) t f
 = B S (B (B S) (B (B p))) q (C q) t f
 = S (B S (B (B S) (B (B p)))) C q t f
 = S (B S (B (B S) (B B B p))) C q t f
 = S (B S (B (B (B S)) (B B B) p)) C q t f
 = S (B (B S) (B (B (B S)) (B B B)) p) C q t f
 = B S (B (B S) (B (B (B S)) (B B B))) p C q t f
 = C (B S (B (B S) (B (B (B S)) (B B B)))) C p q t f
```

However, we can also use `skify`:

```
S (S (K S) (S (K (S (K S))) (S (K (S (K (S (K S))))) (S (K S) (S (K K) (S (K S) K)))))) (K (S (S (K S) (S (K K) S)) (K K)))
```

This simplifies as:

```
C (B S (B (B S) (B (B (B S)) (B S (B K (B S K)))))) (C (B S (B K S)) K)
 = C (B S (B (B S) (B (B (B S)) (B B B)))) (C (B S (B K S)) K)
 = C (B S (B (B S) (B (B (B S)) (B B B)))) C
```

Nice to know a dumb algorithm, tidied up, and my mangled derivations
get to the same solution.

## Their answers

Problem 1's solution is `V f t`, i.e. `V (K I) K`, which is a little
more complex than my solution when expanded, if simpler to see!

I guess their solution also works in a non-extensional setting,
whereas mine just reduces to something that behaves the same.

Looking through their solutions, I've got to admit theirs are somewhat
simpler. I'd been thinking in terms of something like binary decision
diagrams, and composing functions, but their solution is pretty neat.
Ah well.
