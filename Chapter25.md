# Chapter 25: Is There an Ideal Bird?

This chapter is a weird mix of confusing notation and implicit types.
Also, as I'm not using TeX I can't really do all the notation. So,
I'll do a simplified version, with more explicit types.

For types, the basic types are `Natural` for natural numbers, and
`Expr` for expressions. However, the types are used to encode other
things, and the way I'll manage that is to put the encoded type in
brackets. For example, the natural number representing the Goedel
encoding of the expression representing a number would be
`Natural(Expr(Natural))`.

I'll use `~n` to represent the expression that represents the number
`n`. Its type is `~ :: Natural(a) -> Expr(Natural(a))`.

I'll use `G :: Expr(a) -> Natural(Expr(a))` to represent the function
that Godel-encodes an expression.

I'll use a prefix `# :: Natural(a) -> Natural(Expr(Natural(a)))` to
represent the postfix superscript # used in the book to get the
numeral of a number.

I'll use `U x` to represent the upper brackets around `x` in Problem 2
(I'll defer the type until then).

### Problem 1

From the definitions above, we can see that `# n = G (~ n)`.

We're being asked to find a `d :: Expr(Natural a) ->
Expr(Natural(Expr(Natural a)))`. This is quite convoluted, isn't it?!
It is the `Expr`-space equivalent of `#` - if `m = # n`, then we want
`~ m = d (~ n)`.

Put another way:

```
m = # n
m = G (~ n)
~ m = ~ (G (~ n))
~ m = (B ~ G) (~ n)

d = B ~ G
```

This will come in useful in the next problem.

In the meantime, more directly:

```
d n = Z n (~0) (concat (~3) (concat s (concat (d (P n)) (~4))))
```

### Problem 2

`U :: Expr(a) -> Expr(Natural(Expr(a)))`

We want to find a combinator `D :: Expr(Natural(Expr(a)) ->
Expr(Natural(Expr(a)))` that implements `\X -> X (U X)`.

We have

```
D ~n = ~(n * #n)
     = concat (~n) ~(#n)
     = concat (~n) (d (~n))

D x = concat (~3) (concat x (concat (d x) (~4)))
```

### Problem 3

`D`, applied to `U D` is a term which generates its own Goedel number.

The generalisation is that `B A D`, applied to `U (B A D)` will fulfil
this:

```
X = B A D (U (B A D))
  = A (D (U (B A D)))
  = A (U ((B A D) (U (B A D))))
  = A (U (B A D (U (B A D))))
  = A (U X)
```

QED!

### Problem 4

TODO
