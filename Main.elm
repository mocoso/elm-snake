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

main = Html.program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

type alias Model =
  { snake : Snake
  , mouseCoord : Coord
  , width : Int
  , height : Int }

view : Model -> Html.Html Msg
view model =
  Html.body []
    [ Html.h1 [] [ Html.text "Snake" ]
    , Html.div [myStyle] [ Collage.collage model.width model.height (Snake.draw model.snake) |> Element.toHtml ]
    ]

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
  Tick Time.Time

init : ( Model, Cmd Msg )
init =
  ( { snake = { head = Coord 50.0 50.0
              , tail = []
              , length = 70
              , direction = 0.0
              , speed = 4.0 }
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

