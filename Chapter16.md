# Chapter 16: The Forest Without A Name

Laws 1-3 can be written as

```
e -> y AND x -> ~e AND e -> ~x AND (~x AND y) -> e
```

Let's just write this out as a truth table, rather than rearrange:

```
x y e
-----
0 0 0
0 1 1
1 0 0
1 1 0
```

Law 4 says that for all `x`, exists `y` s.t. `e y x = y`. The truth
table entries where this works is `x = FALSE`, for all `x`, so
everything's false.

This is just some symmetry of Chapter 14. Meh.
