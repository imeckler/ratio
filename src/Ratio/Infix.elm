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

{-| Synonym for Ratio.eq -}
(|==|) : Rational -> Rational -> Bool
(|==|) a b = 
  eq a b

{-| Synonym for Ratio.eq where the second operand is an Int -}
(|==) : Rational -> Int -> Bool
(|==) a b = 
  eq a (fromInt b)

{-| Synonym for Ratio.ne  -}
(|/=|) : Rational -> Rational -> Bool
(|/=|) a b = 
  ne a b

{-| Synonym for Ratio.ne where the second operand is an Int  -}
(|/=) : Rational -> Int -> Bool
(|/=) a b = 
  ne a (fromInt b)

{-| Synonym for Ratio.gt -}
(|>|) : Rational -> Rational -> Bool
(|>|) a b = 
  gt a b

{-| Synonym for Ratio.ge -}
(|>=|) : Rational -> Rational -> Bool
(|>=|) a b = 
  ge a b

{-| Synonym for Ratio.lt  -}
(|<|) : Rational -> Rational -> Bool
(|<|) a b = 
  lt a b

{-| Synonym for Ratio.le -}
(|<=|) : Rational -> Rational -> Bool
(|<=|) a b = 
  le a b

-- arithmetic ops

{-| Synonym for Ratio.add where the first operand is an Int -}
(+|) : Int -> Rational -> Rational
(+|) i r = 
  add (fromInt i) r

{-| Synonym for Ratio.add where the second operand is an Int -}
(|+) : Rational -> Int -> Rational
(|+) =
  flip (+|)

{-| Synonym for Ratio.add -}
(|+|) : Rational -> Rational -> Rational
(|+|) =
  add

{-| Synonym for Ratio.subtract where the first operand is an Int  -}
(-|) : Int -> Rational -> Rational
(-|) i r = 
  subtract (fromInt i)  r

{-| Synonym for Ratio.subtract where the second operand is an Int -}
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

{-| Synonym for Ratio.subtract -}
(|-|) : Rational -> Rational -> Rational
(|-|) =
  subtract

{-| Synonym for Ratio.multiply where the second operand is an Int -}
(|*) : Rational -> Int -> Rational
(|*) =
  multiplyByInt

{-| Synonym for Ratio.multiply where the first operand is an Int -}
(*|) : Int -> Rational -> Rational
(*|) = 
  flip multiplyByInt

{-| Synonym for Ratio.multiply -}
(|*|) : Rational -> Rational -> Rational
(|*|) =
  multiply

{-| Synonym for Ratio.divide where the second operand is an Int -}
(|/) : Rational -> Int -> Rational
(|/) =
  divideByInt

{-| Synonym for Ratio.divide where the first operand is an Int -}
(/|) : Int -> Rational -> Rational
(/|) = 
  divideIntBy

{-| Synonym for Ratio.divide -}
(|/|) : Rational -> Rational -> Rational
(|/|) =
  divide



