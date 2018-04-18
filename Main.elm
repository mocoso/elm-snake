import Color
import Collage
import Element
import Html
import Html.Attributes
import Mouse
import Window
import Task
import Time

main = Html.program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

type alias Coord = { x : Float, y : Float }
type alias Tail = List Coord
type alias Model =
  { head : Coord
  , tail : Tail
  , direction : Float
  , speed : Float
  , length : Int
  , mouseCoord : Coord
  , width : Int
  , height : Int }

view : Model -> Html.Html Msg
view model =
  Html.body []
    [ Html.h1 [] [ Html.text "Snake" ]
    , Html.div [myStyle] [ Collage.collage model.width model.height (drawSnake model) |> Element.toHtml ]
    ]

canvasCoord width height mousePosition =
  Coord
    (negate ((toFloat width) / 2) + (toFloat mousePosition.x))
    (((toFloat height) / 2) - (toFloat mousePosition.y))

drawSnake : Model -> List Collage.Form
drawSnake model =
  List.map (drawSnakePart 15 Color.blue) model.tail
  ++ [ drawSnakePart 16 Color.red model.head ]

drawSnakePart : Float -> Color.Color -> Coord -> Collage.Form
drawSnakePart size color coord =
  Collage.filled color (Collage.circle size)
  |> Collage.move (coord.x, coord.y)


advanceSnake : Model -> Model
advanceSnake model =
  let
    (x, y) = fromPolar(model.speed, model.direction)
  in
    { model |
      head = Coord (model.head.x + x) (model.head.y + y)
    , tail = [ model.head ] ++ model.tail }

trimSnakeTail model =
  { model | tail = List.take model.length model.tail }

setSnakeDirection : Model -> Model
setSnakeDirection model =
  let
    (_, newDirection) = toPolar ( (model.mouseCoord.x - model.head.x), (model.mouseCoord.y - model.head.y) )
  in
    { model |
      direction = newDirection }

setMouseCoord : Model -> Mouse.Position -> Model
setMouseCoord model mousePosition =
  { model |
    mouseCoord = canvasCoord model.width model.height mousePosition }


update msg model =
  case msg of
    SetMouseMove mousePosition ->
      ( setMouseCoord model mousePosition, Cmd.none )
    Tick time ->
      ( setSnakeDirection model
      |> advanceSnake
      |> trimSnakeTail
      , Cmd.none )

subscriptions _ =
  Sub.batch
    [ Mouse.moves SetMouseMove
    , Time.every (50 * Time.millisecond) Tick
    ]

type Msg =
  SetMouseMove Mouse.Position |
  Tick Time.Time

init : ( Model, Cmd Msg )
init =
  ( { head = Coord 50.0 50.0
    , tail = []
    , length = 70
    , direction = 0.0
    , speed = 1.0
    , mouseCoord = Coord 0.0 0.0
    , width = 800
    , height = 600
    }
  , Cmd.batch []
  )

myStyle =
  Html.Attributes.style
    [ ("width", "800")
    , ("height", "600")
    , ("position", "fixed")
    , ("top", "0")
    , ("left", "0")
    ]

