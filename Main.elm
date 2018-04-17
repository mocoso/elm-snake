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

type alias Coord = ( Float, Float )
type alias Tail = List Coord
type alias Model =
  { head : Coord
  , tail : Tail
  , velocityX : Float
  , velocityY : Float
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

canvasX width mouseX = negate ((toFloat width) / 2) + (toFloat mouseX)
canvasY height mouseY = ((toFloat height) / 2) - (toFloat mouseY)

drawSnake : Model -> List Collage.Form
drawSnake model =
  List.map (drawSnakePart 15 Color.blue) model.tail
  ++ [ drawSnakePart 16 Color.red model.head ]

drawSnakePart : Float -> Color.Color -> Coord -> Collage.Form
drawSnakePart size color position =
  Collage.filled color (Collage.circle size)
  |> Collage.move position


advanceSnake : Model -> Model
advanceSnake model =
  { model |
    head = ( Tuple.first model.head + model.velocityX, Tuple.second model.head + model.velocityY )
  , tail = [ model.head ] ++ model.tail }

trimSnakeTail model =
  { model | tail = List.take model.length model.tail }

setSnakeDirection : Model -> Model
setSnakeDirection model =
  { model |
    velocityX = (Tuple.first model.mouseCoord - Tuple.first model.head) / 100,
    velocityY = (Tuple.second model.mouseCoord - Tuple.second model.head) / 100}

setMouseCoord model mousePosition =
  { model |
    mouseCoord = ( ( canvasX model.width mousePosition.x ), ( canvasY model.height mousePosition.y) ) }


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
  ( { head = ( 50.0, 50.0 )
    , tail = []
    , length = 70
    , velocityX = 0.0
    , velocityY = 0.0
    , mouseCoord = ( 0.0, 0.0 )
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

