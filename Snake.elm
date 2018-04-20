module Snake exposing (..)

import Coord exposing (Coord)
import Radian
import Collage
import Color

type alias Tail = List Coord
type alias Snake =
  { head : Coord
  , tail : Tail
  , direction : Float
  , speed : Float
  , length : Int }

advance : Snake -> Snake
advance snake =
  let
    (x, y) = fromPolar(snake.speed, snake.direction)
  in
    { snake |
      head = Coord (snake.head.x + x) (snake.head.y + y)
    , tail = [ snake.head ] ++ snake.tail }

trimTail snake =
  { snake | tail = List.take snake.length snake.tail }

setDirection : Coord -> Snake -> Snake
setDirection mouseCoord snake =
  let
    (_, directionToMouse) = toPolar ( (mouseCoord.x - snake.head.x), (mouseCoord.y - snake.head.y) )
  in
    { snake |
      direction = Radian.turnTowards snake.direction directionToMouse 0.06 }

draw : Snake -> List Collage.Form
draw snake =
  List.map (drawSegment 15 Color.blue) snake.tail
  ++ [ drawSegment 16 Color.red snake.head ]

drawSegment : Float -> Color.Color -> Coord -> Collage.Form
drawSegment size color coord =
  Collage.filled color (Collage.circle size)
  |> Collage.move (coord.x, coord.y)

