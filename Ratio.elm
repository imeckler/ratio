module Ratio
    exposing
        ( gcd
        , add
        , multiply
        , divide
        , negate
        , Rational (..)
        , over
        , denominator
        , numerator
        , split
        , toFloat
        , fromInt
        )

{-| A simple module providing a ratio type for rational numbers 

# Types
@docs Rational

# Introduction
@docs over, fromInt

# Operations
@docs add, multiply, divide, negate

# Elimination
@docs numerator, denominator, split, toFloat

# Util
@docs gcd 

-}

import Basics exposing (..)

{-| "Arbitrary" (up to `max_int` size) precision fractional numbers. Think of
    it as the length of a rigid bar that you've constructed from a bunch of
    initial bars of the same fixed length
    by the operations of gluing bars together and shrinking a
    given bar so that an integer number of copies of it glues together to
    make another given bar.
-}
type Rational = Rational Int Int

{-| The biggest number that divides both arguments (the greatest common divisor). -}
gcd : Int -> Int -> Int
gcd a b = if b == 0 then a else gcd b (a % b)

normalize (Rational p q) =
  let k = gcd p q * (if q < 0 then -1 else 1) in Rational (p // k) (q // k)

{-| Addition. It's like gluing together two bars of the given lengths. -}
add : Rational -> Rational -> Rational
add (Rational a b) (Rational c d) =
  normalize (Rational (a * d + b * c) (b * d))

{-| Mulitplication. `mulitply x (c / d)` is the length of the bar that you'd get
    if you glued `c` copies of a bar of length `x` end-to-end and then shrunk it
    down enough so that `d` copies of the shrunken bar would fit in the big
    glued bar. -}
multiply : Rational -> Rational -> Rational
multiply (Rational a b) (Rational c d) =
  normalize (Rational (a * c) (b * d))

{-| Division. It's sort of like multiplication! -}
divide : Rational -> Rational -> Rational
divide r (Rational c d) = multiply r (Rational d c)

{-| This doesn't really fit with the bar metaphor but this is multiplication by `-1`. -}
negate : Rational -> Rational
negate (Rational a b) = Rational (-a) b

{-| `over x y` is like `x / y`. -}
over : Int -> Int -> Rational
over x y = normalize (Rational x y)


{-| `fromInt x = over x 1` -}
fromInt : Int -> Rational
fromInt x = x `over` 1

{-| -}
numerator : Rational -> Int
numerator (Rational a _) = a

{-| -}
denominator : Rational -> Int
denominator (Rational _ b) = b

{-| `split x = (numerator x, denominator x)` -}
split : Rational -> (Int, Int)
split (Rational a b) = (a, b)

{-| -}
toFloat : Rational -> Float
toFloat (Rational a b) = Basics.toFloat a / Basics.toFloat b

