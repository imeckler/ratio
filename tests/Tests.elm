module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Proto.Expect exposing (nearlyEqual)
import Fuzz exposing (Fuzzer, intRange, tuple, map)
import Ratio exposing (..)
import Ratio.Infix exposing (..)

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

fuzzRationalIntTrio : Fuzzer (Rational, Int, Int)
fuzzRationalIntTrio =
  Fuzz.tuple3 (fuzzRational, intRange -100 100, intRange -100 100)


expectAlmostEqual : Float -> Float -> Expectation
expectAlmostEqual =
  Proto.Expect.nearlyEqual

{- rejected in favour of the version from @mgold
expectAlmostEqual signedTarget signedTestable =
  let
    target = abs signedTarget
    testable = abs signedTestable
    delta = 
      if (testable > 1000000) then
        0.1
      else if (testable > 1) then
        0.0001
      else
        0.00000001
    lowerBound =  target - delta
    upperBound =  target + delta
    bothInfinite = Basics.isInfinite target && Basics.isInfinite testable
    firstTest = Expect.greaterThan lowerBound testable
  in
    if bothInfinite then
      Expect.pass
    else
      case 
        (Expect.getFailure firstTest) of
           Just _ ->  
             firstTest
           Nothing ->
             Expect.lessThan upperBound testable
-}
             

all : Test
all =
    concat
      [  
        basics
      , comparisons
      , infixComparisons
      , arithmetic
      ]

basics : Test
basics =
  describe "Basic Tests"
    [     
      test "numerator +n" <|
         \() ->
           Expect.equal 1 (numerator (over 1 4)),   
      test "numerator -n" <|
         \() ->
           Expect.equal -1 (numerator (over -1 4)),   
      test "numerator -d" <|
         \() ->
           Expect.equal -1 (numerator (over 1 -4)), 
      test "numerator -n -d" <|
         \() ->
           Expect.equal 1 (numerator (over -1 -4)),
      test "denominator -d" <|
         \() ->
           Expect.equal 4 (denominator (over 1 -4)),   
      test "denominator -n" <|
         \() ->
           Expect.equal 4 (denominator (over -1 4)),
      test "denominator -n -d" <|
         \() ->
           Expect.equal 4 (denominator (over -1 -4)),
      test "denominator 0" <|
         \() ->
           Expect.equal 0 (denominator (over 1 0)),
      test "eq" <|
         \() ->
           Expect.equal True (Ratio.eq (over 1 2) (over 1 2)),
      test "eq vulgar" <|
         \() ->
           Expect.equal True (Ratio.eq (over 2 1) (fromInt 2)),
      test "ne" <|
         \() ->
           Expect.equal True (Ratio.ne (over 1 2) (over 1 3)),
      test "is zero" <|
         \() ->
           Expect.equal True (isZero (over 0 1)),
      test "is not zero" <|
         \() ->
           Expect.equal False (isZero (over 1 1)),   
      test "infinity" <|
         \() ->
           Expect.equal True (Basics.isInfinite (Ratio.toFloat(over 1 0))),   
      test "infinity under mult by infinity" <|
         \() ->
           Expect.equal (over 1 0) (multiply (over 5 1) (over 1 0)),     
      test "subtraction of infinity" <|
         \() ->
           let 
              minusInfinity = 100 -| Ratio.negate (over 1 0)
           in
             Expect.equal True (Basics.isInfinite (Ratio.toFloat(minusInfinity))),   
      test "production of infinity" <|
         \() ->
           let
             bigInt = 2 ^ 31
           in
             Expect.equal (over 1 0) (multiply (over bigInt 1) (over bigInt 0)),     
      test "infinity" <|
         \() ->
           Expect.equal True (Basics.isInfinite (Ratio.toFloat(over 1 0))),
      -- test the expectAlmostEqual function
      test "expectAlmostEqual function 0" <|
         \() ->
           expectAlmostEqual 0.0 0.0,
      test "expectAlmostEqual function 10" <|
         \() ->
           expectAlmostEqual 10 10.00000001,
      test "expectAlmostEqual function 10^6" <|
         \() ->
           expectAlmostEqual 1000000 1000000.0001,
      test "expectAlmostEqual function 10^-6" <|
         \() ->
           expectAlmostEqual 0.000001 0.0000009999,
      -- basic subtraction
      test "8/7 -1 -1" <|
         \() ->
           Expect.equal (over -6 7) ( (over 8 7) |- 1 |- 1),
      test "7/3 -1/6 -1/6" <|
         \() ->
           Expect.equal (over 2 1) ( (over 7 3) |-| (over 1 6) |-| (over 1 6)),
      test "7/3 +1/6 -1/6" <|
         \() ->
           Expect.equal (over 7 3) ( (over 7 3) |+| (over 1 6) |-| (over 1 6)),
      test "7/3 -1/9 +4/9" <|
         \() ->
           Expect.equal (over 8 3) ( (over 7 3) |-| (over 1 9) |+| (over 4 9)),
      -- basic division
      test "8/7 / 1/2" <|
         \() ->
           Expect.equal (over 16 7) ( (over 8 7) |/| (over 1 2)),
      test "0 / 1/3" <|
         \() ->
           Expect.equal (over 0 1) ( 0 /| (over 1 3)),
      test "8/7 / 2 / 3" <|
         \() ->
           Expect.equal (over 4 21) ( (over 8 7) |/ 2 |/ 3 ),
      test "round down pos" <|
         \() ->
           Expect.equal 3 (Ratio.round (over 10 3)),
      test "round down neg" <|
         \() ->
           Expect.equal -3 (Ratio.round (over -10 3)),
      test "round up pos" <|
         \() ->
           Expect.equal 5 (Ratio.round (over 19 4)),
      test "round up neg" <|
         \() ->
           Expect.equal -5 (Ratio.round (over -19 4)),
      test "floor pos" <|
         \() ->
           Expect.equal 4 (Ratio.floor (over 19 4)),
      test "floor neg" <|
         \() ->
           Expect.equal -5 (Ratio.floor (over -19 4)),
      test "ceiling pos" <|
         \() ->
           Expect.equal 5 (Ratio.ceiling (over 19 4)),
      test "ceiling neg" <|
         \() ->
           Expect.equal -4 (Ratio.ceiling (over -19 4)),
      test "truncate pos" <|
         \() ->
           Expect.equal 4 (Ratio.truncate (over 19 4)),
      test "truncate neg" <|
         \() ->
           Expect.equal -4 (Ratio.truncate (over -19 4)) 
 
        ]

infixComparisons : Test
infixComparisons =
  describe "Infix Comparison Tests"
    [     
      test "is |==|" <|
         \() ->
           Expect.equal True ((over 1 2) |==| (over 1 2)),
      test "is |==" <|
         \() ->
           Expect.equal True ((over 3 1) |== 3),
      test "not |==|" <|
         \() ->
           Expect.equal False ((over 1 2) |==| (over 1 3)), 
      test "is |/=|" <|
         \() ->
           Expect.equal True ((over 1 3) |/=| (over 1 2)),
      test "is |/=" <|
         \() ->
           Expect.equal False ((over 3 1) |/= 3),
      test "not |/=|" <|
         \() ->
           Expect.equal True ((over 1 2) |/=| (over 1 3)), 
      test "== in |>=|" <|
         \() ->
           Expect.equal True ((over 1 2) |>=| (over 1 2)),
      test "== in |<=|" <|
         \() ->
           Expect.equal True ((over 1 2) |>=| (over 1 2))
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
             |> Expect.equal ((Ratio.toFloat a) <= (Ratio.toFloat b)),
      fuzz (fuzzRationalPair) "max" <|
        \(a,b) ->
           Ratio.max a b
             |> Ratio.toFloat
             |> Expect.equal (Basics.max (Ratio.toFloat a)  (Ratio.toFloat b)),
      fuzz (fuzzRationalPair) "min" <|
        \(a,b) ->
           Ratio.min a b
             |> Ratio.toFloat
             |> Expect.equal (Basics.min (Ratio.toFloat a)  (Ratio.toFloat b)),
      fuzz (fuzzRationalPair) "compare" <|
        \(a,b) ->
           let 
              f x y =
                if (x |>| y) then 
                  GT
                else if (x |==| y) then
                  EQ
                else
                  LT
           in
             Ratio.compare a b
               |> Expect.equal (f a b)
        ]




{- these tests all fail, but they really pass because I need an approximate equality tester:

      Expect.almostEqual -> Float -> FLoat -> Expectation
-}
arithmetic : Test
arithmetic =
  describe "Arithmetic Checks"
    [   
    -- addition
    fuzz (fuzzRationalIntPair) "|+" <|
        \(r,i) ->
           r |+ i
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat r) + (Basics.toFloat i)), 
     fuzz (fuzzRationalIntPair) "+|" <|
        \(r,i) ->
          i +| r 
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat r) + (Basics.toFloat i)),
     fuzz (fuzzRationalPair) "|+|" <|
        \(a,b) ->
          a |+| b 
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat a) + (Ratio.toFloat b)),
     fuzz (fuzzRationalIntTrio) "transitivity of +" <|
        \(r,i,j) ->
           r |+ i |+ j
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat r) + (Basics.toFloat i) + (Basics.toFloat j)),

    -- subtraction
    fuzz (fuzzRationalIntPair) "|-" <|
        \(r,i) ->
           r |- i
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat r) - (Basics.toFloat i)), 
     fuzz (fuzzRationalIntPair) "-|" <|
        \(r,i) ->
          i -| r 
             |> Ratio.toFloat
             |> expectAlmostEqual ((Basics.toFloat i) - (Ratio.toFloat r)),
     fuzz (fuzzRationalPair) "|-|" <|
        \(a,b) ->
          a |-| b 
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat a) - (Ratio.toFloat b)),
     fuzz (fuzzRationalIntTrio) "transitivity of -" <|
        \(r,i,j) ->
           r |- i |- j
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat r) - (Basics.toFloat i) - (Basics.toFloat j)),
     fuzz (fuzzRationalIntPair) "transitivity of - and +" <|
        \(r,i) ->
           r |- i |+ i
             |> Ratio.toFloat
             |> expectAlmostEqual (Ratio.toFloat r),   
     fuzz (fuzzRationalIntPair) "transitivity of + abd -" <|
        \(r,i) ->
           i +| r |-| r
             |> Ratio.toFloat
             |> expectAlmostEqual (Basics.toFloat i),   
    
     
     -- multiplication
     fuzz (fuzzRationalIntPair) "|*" <|
        \(r,i) ->
           r |* i
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat r) * (Basics.toFloat i)), 
     fuzz (fuzzRationalIntPair) "*|" <|
        \(r,i) ->
          i *| r 
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat r) * (Basics.toFloat i)),
     fuzz (fuzzRationalPair) "|*|" <|
        \(a,b) ->
          a |*| b 
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat a) * (Ratio.toFloat b)),
     fuzz (fuzzRationalIntTrio) "transitivity of *" <|
        \(r,i,j) ->
           r |* i |* j
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat r) * (Basics.toFloat i) * (Basics.toFloat j)),

    -- division
     fuzz (fuzzRationalIntPair) "|/" <|
        \(r,i) ->
           r |/ i
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat r) / (Basics.toFloat i)),
     fuzz (fuzzRationalIntPair) "/|" <|
        \(r,i) ->
          i /| r 
             |> Ratio.toFloat
             |> expectAlmostEqual ((Basics.toFloat i)  / (Ratio.toFloat r) ),
     fuzz (fuzzRationalPair) "|/|" <|
        \(a,b) ->
          a |/| b 
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat a) / (Ratio.toFloat b)),
     fuzz (fuzzRationalIntTrio) "transitivity of /" <|
        \(r,i,j) ->
           r |/ i |/ j
             |> Ratio.toFloat
             |> expectAlmostEqual ((Ratio.toFloat r) / (Basics.toFloat i) / (Basics.toFloat j))
    ]
