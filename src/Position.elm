module Position exposing (..)

type alias Position = { x : Float, y : Float }

distance a b =
  sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)

minDistance a b =
  abs (a.x - b.x) + abs (a.y - b.y)

areWithin a b range =
  minDistance a b < range && distance a b < range

fromTuple (x, y) =
  Position x y

toTuple position =
  (position.x, position.y)
