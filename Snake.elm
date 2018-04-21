module Snake exposing (..)

import Coord exposing (Coord)
import Radian
import Collage
import Color
import List.Extra

type alias Tail = List Coord
type alias Snake =
  { head : Coord
  , tail : Tail
  , direction : Float
  , speed : Float
  , turnRate : Float
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

grow snake =
  { snake | length = snake.length + 3 }

canEatFruit snake fruit =
  Coord.distance snake.head fruit < 20

setDirection : Coord -> Snake -> Snake
setDirection mouseCoord snake =
  let
    (_, directionToMouse) = toPolar ( (mouseCoord.x - snake.head.x), (mouseCoord.y - snake.head.y) )
  in
    { snake |
      direction = Radian.turnTowards snake.direction directionToMouse snake.turnRate }


draw : Snake -> List Collage.Form
draw snake =
  let
    tailColorCycle = List.Extra.cycle (List.length snake.tail) [Color.blue, Color.lightBlue, Color.blue, Color.darkBlue]
  in
    List.map2 (drawSegment 15) tailColorCycle (List.reverse snake.tail)
    ++ [ drawSegment 16 Color.red snake.head ]

drawSegment : Float -> Color.Color -> Coord -> Collage.Form
drawSegment size color coord =
  Collage.filled color (Collage.circle size)
  |> Collage.move (coord.x, coord.y)

