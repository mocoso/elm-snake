module Radian exposing (..)

type Direction = Clockwise | AntiClockwise

direction from to =
  if from > to then
    if (from - to) <= pi then
      Clockwise
    else
      AntiClockwise
  else
    if (to - from) <= pi then
      AntiClockwise
    else
      Clockwise

normalise angle =
  if angle > pi then
    angle - (2 * pi)
  else if angle < negate pi then
    angle + (2 * pi)
  else
    angle

turnTowards startDirection targetDirection rate =
  case direction startDirection targetDirection of
    AntiClockwise ->
      normalise (startDirection + rate)
    Clockwise ->
      normalise (startDirection - rate)

