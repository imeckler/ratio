module Ratio.Infix exposing (..)

{-| Experimental infix operators for Rationals

# Infix comparisons
@docs (|==|), (|>|)

# Infix arithmetic
@docs (|*), (*|), (|*|), (|-|)
-}

import Ratio exposing (..)

{- experimental -}
infixl 6 |+
infixl 6 +|
infixl 6 |+|
infixl 6 |-
infixl 6 -|
infixl 6 |-|
infixl 7 |*
infixl 7 *|
infixl 7 |*|
infixl 7 |//
infixl 7 //|
infixl 7 |//|

-- comparison ops

{-| == -}
(|==|) : Rational -> Rational -> Bool
(|==|) a b = 
  eq a b

{-| > -}
(|>|) : Rational -> Rational -> Bool
(|>|) a b = 
  gt a b

-- etc. etc.

-- arithmetic ops

{-| multiply an int by a rational -}
(*|) : Int -> Rational -> Rational
(*|) i r = 
  multiply (fromInt i) r

{-| multiply a rational by an int -}
(|*) : Rational -> Int -> Rational
(|*) =
  flip (*|)

{-| multiply 2 rationals-}
(|*|) : Rational -> Rational -> Rational
(|*|) =
  multiply


{-| subtract 2 rationals-}
(|-|) : Rational -> Rational -> Rational
(|-|) a b =
  add a (Ratio.negate b)

-- etc. etc.
