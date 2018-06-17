# Chapter 18: The Master Forest

Ah, finally, SK combinators! I think I'll feel at home here, with my
memories of undergraduate "Foundations of Functional Programming".

I'll use the usual BirdsGalore.hs code, even though I don't really
need to:

```
Problem 1: I = S K S
Problem 2: M = S I I
Problem 3: T = S (K (S I)) K
Problem 4: B = S (K S) K
```

Problem 3 follows the mechanistic pattern:

```
\x y -> y x
\x   -> S I (K x)
        S (K (S I)) I
```

Problem 4 we can do following the reverse-engineering style of earlier
chapters::

```
\x y z -> x (y z)
\x y z -> (K x z) (y z)
\x y z -> S (K x) y z
\x     -> S (K x)
          S (K S) (\x -> K x)
          S (K S) K
```

In the answer for Problem 3, there appears to be a typo saying `T` is
`S (K (S I))` when it's `S (K (S I)) K`. Principle 3's example seems
pretty mangled, too.

"The Secret" is exactly what we learned in undergrad. We also used `B`
and `C` to shortcut quite so much faffing with recursing into the left
and right side if it was unnecessary. The book suggests programming
the algorithm into one of those "home computer" things if you have
one, so of course I'm going to do exactly that. :)
