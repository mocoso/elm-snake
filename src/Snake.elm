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

headRadius = 16

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
  Position.areWithin snake.head fruit 20

isHeadCollidingWithTail snake =
  List.drop 25 snake.tail
  |> List.any
    (\tailSegment -> Position.areWithin snake.head tailSegment 20)

isCollidingWithWall snake gameSize =
  abs snake.head.x + headRadius > toFloat gameSize.width / 2 ||
    abs snake.head.y + headRadius > toFloat gameSize.height / 2

setDirection : Position -> Snake -> Snake
setDirection mousePosition snake =
  let
    (_, directionToMouse) = toPolar ( (mousePosition.x - snake.head.x), (mousePosition.y - snake.head.y) )
  in
    { snake |
      direction = Radian.turnTowards snake.direction directionToMouse snake.turnRate }


draw : Snake -> Collage.Form
draw snake =
  let
    tailColorCycle = List.Extra.cycle (List.length snake.tail) [Color.green, Color.lightGreen, Color.green, Color.darkGreen, Color.darkGreen]
  in
    List.map2 (drawTailSegment 15) tailColorCycle (List.reverse snake.tail)
    ++ [drawHead snake]
    |> Collage.group

drawHead : Snake -> Collage.Form
drawHead snake =
  Collage.group [ drawFilledCircle Color.lightBlue headRadius, drawEyes snake ]
  |> Collage.rotate snake.direction
  |> Collage.move (Position.toTuple snake.head)

drawEyes : Snake -> Collage.Form
drawEyes snake =
  List.map
    (\coords -> drawFilledCircle Color.white 3.0 |> Collage.move coords)
    ( List.map
      (\angle -> fromPolar (headRadius, angle))
      [0.75, -0.75] )
  |> Collage.group

drawTailSegment : Float -> Color.Color -> Position -> Collage.Form
drawTailSegment size color coord =
  drawFilledCircle color size
  |> Collage.move (coord.x, coord.y)

drawFilledCircle : Color.Color -> Float -> Collage.Form
drawFilledCircle color size =
  Collage.filled color (Collage.circle size)

