module Snake exposing (..)

import Position exposing (Position)
import Radian
import Collage
import Color
import List.Extra

type alias Tail = List Position
type alias Snake =
  { head : Position
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
      head = Position (snake.head.x + x) (snake.head.y + y)
    , tail = [ snake.head ] ++ snake.tail }

trimTail snake =
  { snake | tail = List.take snake.length snake.tail }

grow snake =
  { snake | length = snake.length + 5 }

canEatFruit snake fruit =
  Position.distance snake.head fruit < 20

isHeadCollidingWithTail snake =
  List.any (\tailSegment -> Position.distance snake.head tailSegment < 20) (List.drop 10 snake.tail)

setDirection : Position -> Snake -> Snake
setDirection mousePosition snake =
  let
    (_, directionToMouse) = toPolar ( (mousePosition.x - snake.head.x), (mousePosition.y - snake.head.y) )
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

drawSegment : Float -> Color.Color -> Position -> Collage.Form
drawSegment size color coord =
  Collage.filled color (Collage.circle size)
  |> Collage.move (coord.x, coord.y)

