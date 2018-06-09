# Chapter 12: Mockingbird, Warblers, and Starlings

## More on Mockingbirds, Warblers

Using BirdsGalore.hs:

```
Problem 1: X1 X2 (X1 X2) = B M
Problem 2: L = C B M
Problem 3: L = B W B
Problem 5: X2 X1 X1 = B M R
Problem 6: W = C (B M R)
Problem 7: W = B (T (B M (B B T))) (B B T)
Problem 8: M = W T
Problem 9: X1 X2 X3 X3 = B (C (B M (C C)))
Problem 9: X1 X2 X3 X4 X4 = B (B (C (B M (C C))))
Problem 10: H = B W (B C)
Problem 10: H = B (C (B M (C C))) (B C)
Problem 11: W = C (H (C C))
Problem 11: W = R (H R) R
```

## Starlings, The Starling in Action

The point about "no K!" is rather interesting. Adding "K" to the mix
would add a new ability to delete variables from the expression.
Adding "K" also adds Turing-powerfulness - we get the whole "SKI
combinator" thing.

So, presumably the more limited basis from B, T and M has more limited
computational power, but the book (so far) doesn't say what that is. I
think it's perhaps that an expression will either converge in limited
time to a state that can't reduce any further, or diverge into a
structure representable by a DAG (i.e. a regular, infinite tree).
However, it's not obvious to me how you'd prove that easily (if it's
true :).

```
Problem 12: S = B (B W) G
Problem 12: S = B (B W) G
Problem 12: S = B (B W) (B B C)
Problem 13: H = S (C C)
Problem 13: H = S R
Problem 14: W = C (S (C C) (C C))
Problem 14: W = R (S R R) R
Problem 15: W = S T
Problem 16: M = S T T
```

The summary at the end of the chapter seems to be that "B, [TC],
[MWS], I" forms a useful basis, no matter which bits you pick.

(A basis for what? I think the next chapter starts to help on that.)
