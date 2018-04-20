module RadianTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Radian exposing (..)

directionTests =
  describe "direction"
    [
      test "Anti-Clockwise"
        (\_ -> direction 1.0 1.2 |> Expect.equal AntiClockwise )

    , test "Anti-Clockwise across pi boundary"
        (\_ -> direction (pi - 0.2) ((negate pi) + 0.2) |> Expect.equal AntiClockwise )

    , test "Clockwise"
        (\_ -> direction 1.2 1.0 |> Expect.equal Clockwise )

    , test "Clockwise across 0 boundary"
        (\_ -> direction ((negate pi) + 0.2) (pi - 0.2) |> Expect.equal Clockwise )
    ]

normaliseTests =
  describe "normalise"
    [
      test "already between -pi and pi"
        (\_ -> normalise 1.0 |> Expect.equal 1.0 )

    , test "greater than pi"
        (\_ -> normalise (pi + 1) |> Expect.equal ((negate pi) + 1) )

    , test "less than -pi"
        (\_ -> normalise (-1.5 * pi) |> Expect.equal (0.5 * pi) )
    ]


