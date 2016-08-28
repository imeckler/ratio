module Proto.Expect exposing (nearlyEqual)

{-|

Prototype of possible extensions to Test.Expectation

@docs nearlyEqual

-}

import Test exposing (..)
import Expect exposing (..)
import String exposing (join)

custom : (a -> b -> Result String ()) -> a -> b -> Expectation
custom f expected actual =
  case f expected actual of
    Ok () ->
      Expect.pass

    Err str ->
      [ toString actual
      , "╷"
      , "│ " ++ str
      , "╵"
      , toString expected
      ]
      |> String.join "\n"
      |> Expect.fail

{-| naive and rather arbitrary implementation of the requested Float expectation function almostEqual aka nearlyEqual

    (see https://github.com/elm-community/elm-test/issues/41)
-}
nearlyEqual : Float -> Float -> Expectation
nearlyEqual  =
    custom (\i j ->
      let
        tolerance =
          if (j > 1000000) then
            0.1
          else if (j > 1) then
            0.0001
          else
            0.00000001
      in
        if (Basics.isInfinite i && Basics.isInfinite j) then
          Ok ()
        else if abs (i - j) < tolerance then
          Ok ()
        else
          Err <| "Expect to be nearly equal, tolerance: " ++ toString tolerance
    )


