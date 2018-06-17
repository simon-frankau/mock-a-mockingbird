# Chapter 17: Goedel's Forest

## Problem 1

`x* x*` sings iff `x (x* x*)` does.

So `x'* x'*` sings iff `x' (x'* x'*)` does, and `x (*x'* x'*)`
doesn't.

So `N'* N'*` sings iff `N (N'* N'*)` doesn't.

If `N (N'* N'*)` sings, `N'* N'*` is a nightingale, and sings, which
is a contradiction.

So `N (N'* N'*)` doesn't sing, `N'* N'*` is not a nightingale, and
it sings!

This is pretty cool, as I've come across Goedel's paradox before, but
I've generally linked it to Turing-powerful systems, and this thing is
rather more abstact and minimalist, and still gets the same kind of
result.

## Problem 2

I assume it's `N*' N*'`.

##Problem 3

`A` is a predicate for set membership of `S`.

For the set of singing birds (things that evaluate to true), `A'* A'*`
is a member iff it isn't, and we get Russell's paradox, so it's not a
society.

For any society, `A* A* = A (A* A*)` either sings, or it doesn't. If
it sings, the society contains a bird that sings. If it doesn't, the
society lacks a bird that doesn't. This means there is no society that
contains all the birds that don't sing. Adding negation (condition 2)
means that we can't construct a society that contains all the birds
that do sing.
