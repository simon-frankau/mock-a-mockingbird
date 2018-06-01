# Chapter 10: Is There a Sage Bird?

I spent far too long on this.

We have `A x y = x (y y)` aka `A x = x . M` and `M x = x x`. How do we
construct an expression for a combinator that, given `x` produces `M
(x . M)`? This is `M . A`, but how do we express that in terms of `M`
and `A`?

Of course, we don't. We're not showing a construction of this bird,
only showing that it exists. C1 means that, given that we have `M` and
`A`, we have `M . A`.

Duh.

Of course, this "sage bird" is a fixed-point combinator, which should
be fun for future chapters...
