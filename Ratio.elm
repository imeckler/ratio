module Ratio
  ( gcd
  , add
  , multiply
  , divide
  , negate
  , Ratio
  ) where

{-| A simple module providing a ratio type for rational numbers #-}

type Ratio a = Ratio a a
type alias Rational = Ratio Int

gcd : Int -> Int -> Int
gcd a b = if b == 0 then a else gcd b (a % b)

normalize (Ratio p q) =
  let k = gcd p q * (if q < 0 then -1 else 1) in Ratio (p // k) (q // k)

add : Rational -> Rational -> Rational
add (Ratio a b) (Ratio c d) =
  normalize (Ratio (a * d + b * c) (b * d))

multiply : Rational -> Rational -> Rational
multiply (Ratio a b) (Ratio c d) =
  normalize (Ratio (a * c) (b * d))

divide : Rational -> Rational -> Rational
divide r (Ratio c d) = multiply r (Ratio d c)

negate : Rational -> Rational
negate = multiply (Ratio -1 1)

over : Int -> Int -> Rational
over x y = normalize (Ratio x y)

numerator : Ratio a -> a
numerator (Ratio a _) = a

denominator : Ratio a -> a
denominator (Ratio _ b) = b

split : Ratio a -> (a, a)
split (Ratio a b) = (a, b)

