# Chapter 24: Birds That Can Do Arithmetic

```
0 x = I x = x
1 x = V (K I) I x = x (K I) I
2 x = V (K I) (V (K I) I) x = x (K I) (V (K I) I)
```

So, uh, second argument is the predecessor? Meh. I rather like the the
"*n* means apply a function *n* times" approach, but let's see if
there's a good reason for this approach...

### Problem 1

If `V f A = V f B`, then:

```
V f A (C K) = V f B (C K)
C K f A = C K f B
K A f = K B f
A = B
```

So, we just need to prove that `I != V f I`:

```
I K = V (K I) I K
K = K (K I) I
K = K I
```

Contradiction, they're distinct, and so all the numbers are distinct.

### Problem 2

From Problem 1, we have `Z = C K`.

### Problem 3

If a number is non-zero, it's of the form `\x -> x f y`, where `y` is
the predecessor. If it's zero, it's `\x -> x`. Fortunately enough,
passing in `t` gives us `t f y = f` for the non-zero case, and `t` if
it's zero.

So, `Z = T t`.

Now I understand why it's constructed this way. It still feels very
much like it's punning things rather than being consistently designed,
but I see their plan. I think.

### Problem 4

`A = Z`. Not much of a problem, I think it's just a hint to help
people join the dots for following exercises.

### Problem 5

(It's interesting that adding 5 is deemed difficult, since it can just
be unfolded into a great big `V f (V f (V f...` expression - no
recursion needed.)

```
A n = Z n 5 (V f (A (P n)))

A' a n = Z n 5 (V f (a (P n)))
       = Z n 5 (V f (B a P n))
       = Z n 5 (B (V f) (B a P) n)
       = S (C Z 5) (B (V f) (B a P))
A' a   = S (C Z 5) (B (V f) (B a P))
       = S (C Z 5) (B (V f) (C B P a))
       = S (C Z 5) (B (B (V f)) (C B P) a)
A'     = B (S (C Z 5)) (B (B (V f)) (C B P)

A = Theta (B (S (C Z 5)) (B (B (V f)) (C B P))
```

Turing thought of using the fixed-point approach to basically invent
functional programming in combinators?! I keep learning more awesome
things he thought of.

I notice they don't bother actually deriving the combinator properly,
so I won't either:

```
+ m n = Z n m (V f (a (P n)))
```

### Problem 6

`* m n = Z n 0 (+ m (* m (P n)))`

### Problem 7

`e n m = Z m 1 (* n (e n (P m)))`

