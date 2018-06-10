# Chapter 12: Mockingbird, Warblers, and Starlings

## More on Mockingbirds, Warblers

Using BirdsGalore.hs:

```
Problem 1: X1 X2 (X1 X2) = B M
Problem 2: L = C B M
Problem 3: L = B W B
Problem 4: L = Q M
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

## Extra exercises

NB: Gamma should be defined `Gamma x y z w v = y (z w) (x y z w v)`.

The first few exercises can be done from BirdsGalore.hs:

```
Problem 1: G1 = B (B B C)
Problem 1: X1 X4 (X1 X4) (X2 X3) = G1 (B M)
Problem 1: I2 = B (T I) (T I)
Problem 1: I2 (F X1) -> X1 vs X1 - OK!
Problem 1: G1 (B M) F (Q I2) X1 X2 -> X1 X2 X2 vs X1 X2 X2 - OK!
Problem 2: B (B (B W) C) (B B) X1 X2 X3 -> X1 X3 (X2 X3) vs X1 X3 (X2 X3) - OK!
Problem 3: X1 (X2 X4) (X3 X4) = B (B S) B
Problem 4: X1 (X2 X3) (X2 X4) = B H (B B (B B))
Problem 5: X2 (X3 X4) (X1 X2 X3 X4 X5) = Phi (Phi (Phi B)) B
Problem 5: X1 (X2 X3) (X2 X4) = Phi (Phi (Phi B)) B (K K)
```

### Exercise 6

a) From inspection, `S' = C S`.
b) From inspection, `W = S' I`.

## Exercise 7

I've let computer brute-force search do lot, so let's try this one
manually. Running backwards:

```
S x y z
  = x z (y z)
  = Q y (x z) z
  = (Q x) (Q y) z z
  = W ((Q x) (Q y)) z
  = W (Q Q (Q x) y) z
  = W (Q Q (Q Q) x y) z
  = Q (Q Q (Q Q) x) W y z
  = Q (Q Q (Q Q)) Q x W y z
  = C (Q (Q Q (Q Q)) Q) W x y z
```

So, it's `Qhat = Q (Q Q (Q Q)) Q`.
