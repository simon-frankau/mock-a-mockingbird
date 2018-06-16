# Chapter 13: A Gallery of Sage Birds

## Some Sage Birds, Enter the Queer Bird, Curry's Sage Bird, The Turing Bird, Owls

Using BirdsGalore.hs:

```
Problem 1: B M (R M B)
Problem 2: B M (C B M)
Problem 3: B M L
Problem 4: B M (B W B)
Problem 5: B (W (B (C W) W)) B
Problem 6: L W (Q L)
Problem 8: Q (Q M) M
Problem 9: S L L
Problem 10: W S (B W B)
Problem 11: U = L (S (C T))
Problem 12: U U
Problem 13: O = B W (C B)
Problem 14: L O (L O)
Problem 14: U = L O
Problem 15: M = O I
Problem 16: O = S I
```

## Why Owls Are So Interesting

### Problem 17

`x y = y`, so `x (x y) = x y = y = x y`, and `x` is fond of `x y`.

### Problem 18

`O Theta x = x (Theta x) = x (x (Theta x)) = x (O Theta x)`, so `O
Theta` is a fixed-point combinator.

### Problem 19

`Theta O x = O (Theta O) x = x (Theta O x)`, so `Theta O` is a
fixed-point combinator.

### Problem 20

If `O A = A`, then `A x = O A x = x (A x)`, and `A` is a fixed-point
combinator.

### Problem 21

 `Theta A = A (Theta A)`, so `A` is fond of `Theta A`, so `Theta A` is
 a fixed-point combinator.

### Problem 22

`O Theta x = x (Theta x) = Theta x`, so `O Theta` is similar to
`Theta`.

### Problem 23

In an extensional forest, `O Theta = Theta` if `O Theta` is similar to
`Theta`. It is, from 22, so `Theta` is a fixed point of `O`.

## On "O"

I hadn't come across "O" before. It's pretty neat. It's kind of the
unfixed version of the fixed-point combinator, and I'm sure there's
some neat tricks it can be used for...
