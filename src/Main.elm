import Color
import Collage
import Element
import Html
import Html.Attributes
import Html.Events
import Window
import Json.Decode as Decode
import Task
import Time
import Snake exposing (Snake)
import Position exposing (Position)
import Random

main = Html.program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }


type State = Playing | GameOver

type alias Model =
  { state : State
  , snake : Snake
  , fruit : Position
  , mousePosition : Position
  , score : Int }

emptyNode = Html.text ""

view : Model -> Html.Html Msg
view model =
  Html.body []
    [ Html.div
      [ titleStyle ]
      [ Html.h1 [] [ Html.text "Snake" ]
      , Html.p [] [ Html.text ("Score: " ++ toString model.score) ]
      , (if model.state == GameOver then Html.p [] [ Html.text "Game over" ] else emptyNode )
      , (if model.state == GameOver then Html.p [] [ Html.text "Click Mouse to Start" ] else emptyNode )
      , Html.p [] [
          Html.text "An experiment in Elm - "
        , Html.a [ Html.Attributes.attribute "href" "https://github.com/mocoso/elm-snake" ] [ Html.text "view the source code"] ] ]
    , Html.div
      [ onMouseMove,
        Html.Events.onClick MouseClick,
        gameStyle ]
      [ Collage.collage gameSize.width gameSize.height
        ([drawFruit model.fruit, Snake.draw model.snake])
        |> Element.toHtml ] ]

onMouseMove : Html.Attribute Msg
onMouseMove =
  Html.Events.on "mousemove" (Decode.map2 (\x -> \y -> MouseMove (x, y))
      (Decode.field "offsetX" Decode.float)
      (Decode.field "offsetY" Decode.float))

drawFruit position =
  Collage.filled Color.red (Collage.circle fruitRadius)
  |> Collage.move (position.x, position.y)

canvasPosition width height mousePosition =
  Position
    (negate ((toFloat width) / 2) + (mousePosition.x))
    (((toFloat height) / 2) - (mousePosition.y))

setMousePosition : Model -> Position -> Model
setMousePosition model mousePosition =
  { model |
    mousePosition = canvasPosition gameSize.width gameSize.height mousePosition }


update msg model =
  case msg of
    MouseMove (x, y) ->
      ( setMousePosition model (Position x y), Cmd.none )
    NewFruitPosition (x, y) ->
      ( { model | fruit = Position (toFloat x) (toFloat y) }, Cmd.none )
    Tick time ->
      (tickSnake model, Cmd.batch [])
      |> fruitEatingChecks
      |> collisionChecks
    MouseClick ->
      init


tickSnake model =
  { model |
    snake =
      Snake.setDirection model.mousePosition model.snake
      |> Snake.advance
      |> Snake.trimTail }

fruitEatingChecks (model, commands) =
  if Snake.canEatFruit model.snake model.fruit then
    ( { model |
        snake = Snake.grow model.snake
      , score = model.score + 10 }
    , Cmd.batch[commands, newFruitCmd] )
  else
    (model, commands)

collisionChecks (model, commands) =
  if Snake.isHeadCollidingWithTail model.snake ||
    Snake.isCollidingWithWall model.snake gameSize then
    ( { model |
        state = GameOver }
    , commands )
  else
    (model, commands)

subscriptions model =
  case model.state of
    Playing ->
      Sub.batch
        [ Time.every ((1000.0 / fps) * Time.millisecond) Tick ]
    GameOver ->
      Sub.none

type Msg =
  MouseMove (Float, Float) |
  Tick Time.Time |
  NewFruitPosition (Int, Int) |
  MouseClick

newFruitCmd : Cmd Msg
newFruitCmd =
  let
    halfWidth = floor (toFloat gameSize.width / 2) - (4 * fruitRadius)
    halfHeight = floor (toFloat gameSize.height / 2) - (4 * fruitRadius)
  in
  Cmd.batch
    [ Random.generate NewFruitPosition
      (Random.pair
        (Random.int (negate halfWidth) halfWidth)
        (Random.int (negate halfHeight) halfHeight)) ]

init : ( Model, Cmd Msg )
init =
  ( { state = Playing
    , snake = { head = Position 50.0 50.0
              , tail = []
              , length = snakeInitialLength
              , direction = 0.0
              , speed = snakeSpeed
              , turnRate = snakeTurnRate }
    , fruit = Position 40.0 40.0
    , mousePosition = Position 0.0 0.0
    , score = 0
    }
  , newFruitCmd
  )

titleStyle =
  Html.Attributes.style
  [ ("width", "200px")
  , ("float", "left")
  , ("margin-left", "20px")
  ]

gameStyle =
  Html.Attributes.style
    [ ("width", (toString gameSize.width) ++ "px")
    , ("height", (toString gameSize.height) ++ "px")
    , ("background-color", "black")
    , ("float", "left")
    , ("margin-top", "20px")
    , ("margin-left", "20px")
    ]


gameSize = { width = 600, height = 600 }
fps = 40
snakeSpeed = 150 / fps
snakeTurnRate = ((2 * pi) / 300) * snakeSpeed
snakeInitialLength = round (300 / snakeSpeed)
fruitRadius = 10
