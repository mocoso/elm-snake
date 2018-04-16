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

type alias Model =
  { x : Float
  , y : Float
  , velocityX : Float
  , velocityY : Float
  , width : Int
  , height : Int }

view : Model -> Html.Html Msg
view model =
  Html.body []
    [ Html.h1 [] [ Html.text "Snake" ]
    , Html.div [myStyle] [ Collage.collage model.width model.height [drawSnake model] |> Element.toHtml ]
    ]

canvasX width mouseX = negate ((toFloat width) / 2) + (toFloat mouseX)
canvasY height mouseY = ((toFloat height) / 2) - (toFloat mouseY)

drawSnake : Model -> Collage.Form
drawSnake model =
  Collage.filled Color.red (Collage.circle 20)
    |> Collage.moveX model.x
    |> Collage.moveY model.y

advanceSnake model =
  { model |
    x = model.x + model.velocityX,
    y = model.y + model.velocityY }

setSnakeDirection model mousePosition =
  { model |
    velocityX = ((canvasX model.width mousePosition.x) - model.x) / 100,
    velocityY = ((canvasY model.height mousePosition.y) - model.y) / 100}

update msg model =
  case msg of
    SetMouseMove mousePosition ->
      ( setSnakeDirection model mousePosition, Cmd.none )
    Tick time ->
      ( advanceSnake model, Cmd.none )

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
  ( { x = 50.0
    , y = 50.0
    , velocityX = 0.0
    , velocityY = 0.0
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

