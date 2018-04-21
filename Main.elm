import Color
import Collage
import Element
import Html
import Html.Attributes
import Mouse
import Window
import Task
import Time
import Snake exposing (Snake)
import Coord exposing (Coord)
import Random

main = Html.program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

type alias Model =
  { snake : Snake
  , fruit : Coord
  , mouseCoord : Coord }

view : Model -> Html.Html Msg
view model =
  Html.body []
    [ Html.h1 [] [ Html.text "Snake" ]
    , Html.div
        [myStyle]
        [ Collage.collage gameSize.width gameSize.height
          ([drawFruit model.fruit] ++ Snake.draw model.snake)
          |> Element.toHtml ]
    ]

drawFruit fruitCoord =
  Collage.filled Color.green (Collage.circle 10)
  |> Collage.move (fruitCoord.x, fruitCoord.y)

canvasCoord width height mousePosition =
  Coord
    (negate ((toFloat width) / 2) + (toFloat mousePosition.x))
    (((toFloat height) / 2) - (toFloat mousePosition.y))

setMouseCoord : Model -> Mouse.Position -> Model
setMouseCoord model mousePosition =
  { model |
    mouseCoord = canvasCoord gameSize.width gameSize.height mousePosition }


update msg model =
  case msg of
    SetMouseMove mousePosition ->
      ( setMouseCoord model mousePosition, Cmd.none )
    NewFruitPosition (x, y) ->
      ( { model | fruit = Coord (toFloat x) (toFloat y) }, Cmd.none )
    Tick time ->
      (tickSnake model, Cmd.batch [])
      |> fruitEatingChecks

tickSnake model =
  { model |
    snake =
      Snake.setDirection model.mouseCoord model.snake
      |> Snake.advance
      |> Snake.trimTail }

fruitEatingChecks (model, commands) =
  if Snake.canEatFruit model.snake model.fruit then
    ( { model |
        snake = Snake.grow model.snake }
    , Cmd.batch[commands, newFruitCmd] )
  else
    (model, commands)

subscriptions _ =
  Sub.batch
    [ Mouse.moves SetMouseMove
    , Time.every ((1000.0 / fps) * Time.millisecond) Tick
    ]

type Msg =
  SetMouseMove Mouse.Position |
  Tick Time.Time |
  NewFruitPosition (Int, Int)

newFruitCmd : Cmd Msg
newFruitCmd =
  let
    halfWidth = floor (toFloat gameSize.width / 2)
    halfHeight = floor (toFloat gameSize.height / 2)
  in
  Cmd.batch
    [ Random.generate NewFruitPosition
      (Random.pair
        (Random.int (negate halfWidth) halfWidth)
        (Random.int (negate halfHeight) halfHeight)) ]

init : ( Model, Cmd Msg )
init =
  ( { snake = { head = Coord 50.0 50.0
              , tail = []
              , length = snakeInitialLength
              , direction = 0.0
              , speed = snakeSpeed
              , turnRate = snakeTurnRate }
    , fruit = Coord 40.0 40.0
    , mouseCoord = Coord 0.0 0.0
    }
  , newFruitCmd
  )

myStyle =
  Html.Attributes.style
    [ ("width", toString gameSize.width)
    , ("height", toString gameSize.height)
    , ("position", "fixed")
    , ("top", "0")
    , ("left", "0")
    ]

gameSize = { width = 800, height = 600 }
fps = 20
snakeSpeed = 100 / fps
snakeTurnRate = ((2 * pi) / 300) * snakeSpeed
snakeInitialLength = round (250 / snakeSpeed)
