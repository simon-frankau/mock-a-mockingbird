# Chapter 19: Aristocratic Birds

AKA non-cancellative combinators.

Using BirdsGalore.hs again:

```
Problem 1: J = W (B C (E B C E))
Problem 2: X1 (X3 X2) = J I
Problem 3: T = J I I
Problem 4: R = J T
Problem 5: B = C (J I C) (J I)
Problem 6: J1 = B J T
Problem 7: M = C (C (C J1 T) T) T
```

Note the solutions are a bit approximate as I'm not defining all the
combinators they suggest using, but instead I'm going for the things
they're defined in terms of. It all works ok, anyway.

Looks like there might be a typo in the answer to problem 5?

## J combinator

The J combinator is awesome. I hadn't seen it before, but the way it
kind of unfolds by shovelling I into it to create some other
combinators that can then be combined with it that eventually produces
all these other useful combinators is really very neat.

## Bases

The discussion of BTMI/BCWI-derived combinators is really rather
interesting. If only, on previous attempts I hadn't got caught up in
my own attempts to understand the derivability of different sets of
combinators from bases, maybe I'd have got far enough through the book
to read this! It kind of seems cruel to put this material so far away
from the material that provokes the questions it answers!

## Lambda I and K calculi

One of the things I find strangely frustrating about the book is that
it dresses up topics that I'd actually quite like to read straight.
Talking about "birds" all the time is weird when I want to talk about
combinators. Concepts miss their well-known mathematical names,
references and results are omitted, and it's not clear which
combinators are standard and which ones are specific to the book!

So, it's actually very pleasant when it says "This is I calculus, and
this is K calculus". It's a real shame that when I search for this I
then just end up on the Wikipedia page for combinatory logic, being
referred to Barendregt (which so far I have resisted owning!).

Of course, the same page refers to *To Mock a Mockingbird* as "A
gentle introduction to combinatory logic". Harumph... well, I guess it
is pretty gentle, actually.

The I calculus looks really interesting, as I haven't learnt about it
and, well, it doesen't seem to be discussed much compared to the K
calculus.

## Deriving BCSI combinators

In Chapter 11 I worked out a restricted version all by myself for the
case where you're just using B, C and W. All that work wasted. :) Yet
again, the things I struggled to work out for myself are explained
more easily here! Having said that, I was dealing with a slightly
funkier case where "I" isn't available, so I guess it's not all lost.

Of course, I intend to implement the algorithm mentioned in the book.
