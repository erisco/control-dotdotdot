# Control.DotDotDot

Haskell operator `g ... f = \x1 .. xn -> g (f x1 .. xn)`. Compose
functions such that all arguments are applied. Obviates `(.).(.)` and
similar patterns in some cases.

# Examples

## Add four numbers

    > ((+) ... (+) ... (+)) (1 :: Int) 2 3 4
    10
