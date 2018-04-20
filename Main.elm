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
  , mouseCoord : Coord
  , width : Int
  , height : Int }

view : Model -> Html.Html Msg
view model =
  Html.body []
    [ Html.h1 [] [ Html.text "Snake" ]
    , Html.div
        [myStyle]
        [ Collage.collage model.width model.height
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
    mouseCoord = canvasCoord model.width model.height mousePosition }


update msg model =
  case msg of
    SetMouseMove mousePosition ->
      ( setMouseCoord model mousePosition, Cmd.none )
    NewFruitPosition (x, y) ->
      ( { model | fruit = Coord (toFloat x) (toFloat y) }, Cmd.none )
    Tick time ->
      ( { model |
          snake =
            Snake.setDirection model.mouseCoord model.snake
            |> Snake.advance
            |> Snake.trimTail }
      , Cmd.none )

subscriptions _ =
  Sub.batch
    [ Mouse.moves SetMouseMove
    , Time.every (50 * Time.millisecond) Tick
    ]

type Msg =
  SetMouseMove Mouse.Position |
  Tick Time.Time |
  NewFruitPosition (Int, Int)

newFruitCmd : Cmd Msg
newFruitCmd =
  Cmd.batch
    [ Random.generate NewFruitPosition
      (Random.pair (Random.int -400 400) (Random.int -300 300)) ]

init : ( Model, Cmd Msg )
init =
  ( { snake = { head = Coord 50.0 50.0
              , tail = []
              , length = 70
              , direction = 0.0
              , speed = 4.0 }
    , fruit = Coord 40.0 40.0
    , mouseCoord = Coord 0.0 0.0
    , width = 800
    , height = 600
    }
  , newFruitCmd
  )

myStyle =
  Html.Attributes.style
    [ ("width", "800")
    , ("height", "600")
    , ("position", "fixed")
    , ("top", "0")
    , ("left", "0")
    ]

