module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (Fuzzer, intRange, tuple, map)
import Expect
import String
import Ratio exposing (..)

fuzzRational : Fuzzer Rational
fuzzRational = 
  Fuzz.tuple (intRange -100 100, intRange 1 100) 
    -- |> Fuzz.map (uncurry Rational) -- don't think this is a safe constructor after all
    |> Fuzz.map (\(a,b) -> over a b)

fuzzRationalPair : Fuzzer (Rational, Rational)
fuzzRationalPair =
  Fuzz.tuple (fuzzRational, fuzzRational)

fuzzRationalIntPair : Fuzzer (Rational, Int)
fuzzRationalIntPair =
  Fuzz.tuple (fuzzRational, intRange -100 100)

all : Test
all =
    concat
      [  
        basics
      , comparisons
      , arithmetic
      ]

basics : Test
basics =
  describe "Basic Tests"
    [     
      test "numerator +n" <|
         \() ->
           Expect.equal (numerator (over 1 4)) 1,   
      test "numerator -n" <|
         \() ->
           Expect.equal (numerator (over -1 4)) -1,   
      test "numerator -d" <|
         \() ->
           Expect.equal (numerator (over 1 -4)) -1, 
      test "numerator -n -d" <|
         \() ->
           Expect.equal (numerator (over -1 -4)) 1,
      test "denominator -d" <|
         \() ->
           Expect.equal (denominator (over 1 -4)) 4,   
      test "denominator -n" <|
         \() ->
           Expect.equal (denominator (over -1 4)) 4,
      test "denominator -n -d" <|
         \() ->
           Expect.equal (denominator (over -1 -4)) 4,
      test "denominator 0" <|
         \() ->
           Expect.equal (denominator (over 1 0)) 0,
      test "eq" <|
         \() ->
           Expect.equal (over 1 2) (over 1 2),
      test "eq vulgar" <|
         \() ->
           Expect.equal (over 2 1) (fromInt 2),
      test "ne" <|
         \() ->
           Expect.notEqual (over 1 2) (over 1 3),
      test "is zero" <|
         \() ->
           Expect.equal (isZero (over 0 1)) True,
      test "is not zero" <|
         \() ->
           Expect.equal (isZero (over 1 1)) False     
 
        ]

comparisons : Test
comparisons =
  describe "Comparison Checks"
    [        
      fuzz (fuzzRationalPair) ">" <|
        \(a,b) ->
           gt a b
             |> Expect.equal ((Ratio.toFloat a) > (Ratio.toFloat b)),
      fuzz (fuzzRationalPair) "<" <|
        \(a,b) ->
           lt a b
             |> Expect.equal ((Ratio.toFloat a) < (Ratio.toFloat b)),
      fuzz (fuzzRationalPair) ">=" <|
        \(a,b) ->
           ge a b
             |> Expect.equal ((Ratio.toFloat a) >= (Ratio.toFloat b)),
      fuzz (fuzzRationalPair) "<=" <|
        \(a,b) ->
           le a b
             |> Expect.equal ((Ratio.toFloat a) < (Ratio.toFloat b)),
      fuzz (fuzzRationalPair) "max" <|
        \(a,b) ->
           Ratio.max a b
             |> Ratio.toFloat
             |> Expect.equal (Basics.max (Ratio.toFloat a)  (Ratio.toFloat b)),
      fuzz (fuzzRationalPair) "min" <|
        \(a,b) ->
           Ratio.min a b
             |> Ratio.toFloat
             |> Expect.equal (Basics.min (Ratio.toFloat a)  (Ratio.toFloat b))

        ]


{- these tests all fail, but they really pass because I need an approximate equality tester:

      Expect.almostEqual -> Float -> FLoat -> Expectation
-}
arithmetic : Test
arithmetic =
  describe "Arithmetic Checks"
    [        
     fuzz (fuzzRationalIntPair) "*|" <|
        \(r,i) ->
           r *| i
             |> Ratio.toFloat
             |> Expect.equal ((Ratio.toFloat r) * (Basics.toFloat i)), 
     fuzz (fuzzRationalIntPair) "|*" <|
        \(r,i) ->
          i |* r 
             |> Ratio.toFloat
             |> Expect.equal ((Ratio.toFloat r) * (Basics.toFloat i)),
     fuzz (fuzzRationalPair) "|*|" <|
        \(a,b) ->
          a |*| b 
             |> Ratio.toFloat
             |> Expect.equal ((Ratio.toFloat a) * (Ratio.toFloat b)),
     fuzz (fuzzRationalIntPair) "transitivity of *" <|
        \(r,i) ->
           r *| i *| i
             |> Ratio.toFloat
             |> Expect.equal ((Ratio.toFloat r) * (Basics.toFloat i) * (Basics.toFloat i))
    ]
