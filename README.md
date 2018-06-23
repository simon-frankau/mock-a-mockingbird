# To Mock a Mockingbird

These are my notes on the solutions to the questions in Raymond
Smullyan's *To Mock a Mockingbird*, which is a fantastic set of
exercises around combinatory logic, disguised as puzzles about birds.

Ignore the first section of the book, it's bog-standard and not
terribly exciting logic puzzles. The combinatory logic puzzles are
awesome, though.

I first came across combinatory logic as part of my undergrad course,
effectively as an addendum on lambda calculus. I found it fascinating
(although I also found lambda calculus fascinating!). Fast-forward a
few years, and this book was recommended to me by a colleague (along
with a huge tome on the history of the oil industry, and *Lolita*
- and it turns out that the oil book was the easiest to read!).

*To Mock a Mockingbird* is awesome, but I never bashed all the way
through it. This is my upteenth go at it. The exercises all have
answers provided in the book, but I felt it was worth recording my own
thoughts and approach.

## Chapters

I'm only covering the combinator parts of the book:

 * [Chapter 9](Chapter9.md)
 * [Chapter 10](Chapter10.md)
 * [Chapter 11](Chapter11.md)
 * [Chapter 12](Chapter12.md)
 * [Chapter 13](Chapter13.md)
 * [Chapter 14](Chapter14.md)
 * [Chapter 15](Chapter15.md)
 * [Chapter 16](Chapter16.md)
 * [Chapter 17](Chapter17.md)
 * [Chapter 18](Chapter18.md)
 * [Chapter 19](Chapter19.md)
 * [Chapter 20](Chapter20.md)
 * [Chapter 21](Chapter21.md)
 * [Chapter 22](Chapter22.md)
 * [Chapter 23](Chapter23.md)
 * [Chapter 24](Chapter24.md)
 * [Chapter 25](Chapter25.md)

## The Code

The code I've used to solve the questions is horrible. I'm getting
used to writing mediocre code and just publishing it rather than
either spending time agonising over it or just not publishing it. It's
ok to just write it and leave it, sometimes. Just tidy it up before
making it the core of a production system! :)

## Closing thoughts

Having finally worked through the book, I feel I was fighting it too
much. There are some really horrible puzzles very early on - finding a
fixed-point combinator right at the start, for example. A bunch of the
other "find a combinator" puzzles are either very simple, or
occasionally very hard (find a convoluted combinator), and building
code to search allowed me to skip the tedium, keep momentum and focus
on understanding.

My previous attempts at the book got sidetracked by interesting
questions about bases and other results the exercises didn't ask for,
although I was distracted anyway. This time, I ploughed through and
discovered that a) it was actually a lot easier than I expected once I
got going, and b) some of those questions got answered by the book
later, so I didn't need to work them out for myself!

It was a shame the book didn't talk more about the I-calculus, as it's
something I know little about. Indeed, generally it's a shame that it
doesn't just talk directly about combinators a bit more, and the
historical approach to the subject. The history's probably quite messy
and confusing, though. The tidied-up subject once it's properly
understood is generally a lot more approachable, even though in this
case it's been made untidy again!

Then, the book ends with some Goedel. These books always do, and I've
kind of got used to it since reading Goedel Escher Bach somewhat over
20 years ago. In some ways, this is something of a let down to me.
Once again, we've abstractly shown what we can't do. Why can't we show
what we can? Write more combinator programs! A nice SK-term-evaluator
written in SK combinators would be fun. Or throw in the connection to
lambda calculus.

I'm wrong, though. It should end with Goedel for a reason. That's the
structure of these books, and it's very good at what it does. It's
still enjoyable and worthwhile, and I wish I'd just managed to get it
polished off ages ago!
