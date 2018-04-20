module Coord exposing (..)

type alias Coord = { x : Float, y : Float }

distance a b =
  sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)
