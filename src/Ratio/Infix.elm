module Ratio.Infix exposing (..)

{-| Experimental infix operators for Rationals

# Infix comparisons
@docs (|==|), (|==), (|/=|), (|/=), (|>|), (|>=|), (|<|), (|<=|)

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
infixl 7 |/
infixl 7 /|
infixl 7 |/|

-- comparison ops

{- we won't provide |> or <| for comparing Rationals with Ints because of
   confusion with the same infix functions in Basics
-}

{-| Rational == Rational -}
(|==|) : Rational -> Rational -> Bool
(|==|) a b = 
  eq a b

{-| Rational == Int -}
(|==) : Rational -> Int -> Bool
(|==) a b = 
  eq a (fromInt b)

{-| Rational /= Rational -}
(|/=|) : Rational -> Rational -> Bool
(|/=|) a b = 
  ne a b

{-| Rational /= Int -}
(|/=) : Rational -> Int -> Bool
(|/=) a b = 
  ne a (fromInt b)

{-|Rational  > Rational -}
(|>|) : Rational -> Rational -> Bool
(|>|) a b = 
  gt a b


{-| Rational >= Rational -}
(|>=|) : Rational -> Rational -> Bool
(|>=|) a b = 
  ge a b

{-| Rational < Rational -}
(|<|) : Rational -> Rational -> Bool
(|<|) a b = 
  lt a b

{-| Rational <= Rational -}
(|<=|) : Rational -> Rational -> Bool
(|<=|) a b = 
  le a b

-- arithmetic ops

{-| add an int to a rational -}
(+|) : Int -> Rational -> Rational
(+|) i r = 
  add (fromInt i) r

{-| add a rational to an int -}
(|+) : Rational -> Int -> Rational
(|+) =
  flip (+|)

{-| add 2 rationals-}
(|+|) : Rational -> Rational -> Rational
(|+|) =
  add

{-| subtract an int from a rational -}
(-|) : Int -> Rational -> Rational
(-|) i r = 
  subtract (fromInt i)  r

{-| subtract a rational from an int -}
(|-) : Rational -> Int -> Rational
(|-) r i =
  subtract r (fromInt i) 

{-
  -- flip not used in arithmetic because it seems to give the wrong precedence in expressions
  -- not got to the cause of this yet
(|-) : Rational -> Int -> Rational
(|-) =
  flip (-|) 
-}

{-| subtract 2 rationals-}
(|-|) : Rational -> Rational -> Rational
(|-|) =
  subtract

{-| multiply a rational by an int -}
(|*) : Rational -> Int -> Rational
(|*) =
  multiplyByInt

{-| multiply an int by a rational -}
(*|) : Int -> Rational -> Rational
(*|) = 
  flip multiplyByInt

{-| multiply 2 rationals-}
(|*|) : Rational -> Rational -> Rational
(|*|) =
  multiply

{-| divide a rational by an int -}
(|/) : Rational -> Int -> Rational
(|/) =
  divideByInt

{-| divide an int by a rational -}
(/|) : Int -> Rational -> Rational
(/|) = 
  divideIntBy

{-| divide a rational by another-}
(|/|) : Rational -> Rational -> Rational
(|/|) =
  divide



