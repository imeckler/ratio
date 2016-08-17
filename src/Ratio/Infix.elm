module Ratio.Infix exposing (..)

{-| Experimental infix operators for Rationals

# Infix
@docs (|*), (*|), (|*|)
-}

import Ratio exposing (Rational, multiply, divide, fromInt)

{- experimental -}
infixl 7 |*
infixl 7 *|
infixl 7 |*|

{-| multiply an int by a rational -}
(|*) : Int -> Rational -> Rational
(|*) i r = 
  multiply (fromInt i) r

{-| multiply a rational by an int -}
(*|) : Rational -> Int -> Rational
(*|) =
  flip (|*)

{-| multiply 2 rationals-}
(|*|) : Rational -> Rational -> Rational
(|*|) =
  multiply
