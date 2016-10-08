module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.App as Html
import AnimationFrame
import Time exposing (Time)
import Keyboard exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)


-- MODEL

type alias Height = Int
type alias Width = Int

type alias X = Float
type alias Y = Float
type alias Position = (X, Y)


type Direction
 = North
 | East
 | South
 | West


type alias Snake =
    { head: Position
    , tail: List Position
    , direction: Direction
    }


type alias Apple =
    { position: Position
    }


type GameState = Pre | Playing | Over


type alias Model =
    { snake: Snake
    , gameState: GameState
    }


init : ( Model, Cmd Msg )
init =
    ( { snake = { head = (0, 0)
                , tail = [ (0, 1) ]
                , direction = North
                }
      , gameState = Pre
      }
      , Cmd.none )


-- MESSAGES


type Msg
    = TimeUpdate Time
    | StartGame
    | Move Direction
    | EndGame
    | Noop


-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            (
              { model
              | snake = animateSnake model.snake
              , gameState = hasSnakeDied model.snake displayWidth displayHeight
              }
            , Cmd.none )

        StartGame ->
            ( { model | gameState = Playing }
            , Cmd.none )

        EndGame ->
            ( { model | gameState = Over }
            , Cmd.none )

        Move direction ->
            ( { model | snake = changeDirection model.snake direction }
            , Cmd.none )

        Noop -> ( model, Cmd.none )


changeDirection : Snake -> Direction -> Snake
changeDirection { head, tail, direction } newDirection =
    { head = head
    , tail = tail
    , direction = newDirection
    }


animateSnake : Snake -> Snake
animateSnake { head, tail, direction } =
    let
        move = functionFromDirection direction
    in
        { head = move head
        , tail = List.map move tail
        , direction = direction
        }


-- The game is over when the snake hits a wall
hasSnakeDied : Snake -> Width -> Height -> GameState
hasSnakeDied { head, tail, direction } displayWidth displayHeight =
    let
        (x, y) = head
        collisionBuffer = -10
        maxX = toFloat displayWidth / 2 + collisionBuffer
        minX = negate maxX
        maxY = toFloat displayHeight / 2 + collisionBuffer
        minY = negate maxY
    in
        if x > maxX || x < minY || y > maxY || y < minY then
            Over
        else
            Playing




functionFromDirection : Direction -> (Position -> Position)
functionFromDirection direction =
    case direction of
        North -> north
        South -> south
        East -> east
        West -> west


north : Position -> Position
north (x, y) =
    (x, y + 1)


south : Position -> Position
south (x, y) =
    (x, y - 1)


east : Position -> Position
east (x, y) =
    (x + 1, y)


west : Position -> Position
west (x, y) =
    (x - 1, y)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.gameState == Playing then
            AnimationFrame.diffs TimeUpdate
          else
            Sub.none
        , Keyboard.downs keyToMsg
        ]



keyToMsg : KeyCode -> Msg
keyToMsg keycode =
    case keycode of
        32 -> StartGame
        87 -> Move North
        83 -> Move South
        65 -> Move West
        68 -> Move East
        _  -> Noop


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ rendergameState model
        , renderDisplay model
        ]


rendergameState : Model -> Html Msg
rendergameState model =
    div [ style
            [ "position" => "absolute"
            , "bottom" => "0"
            , "left" => "0"
            ]
        ]
        [ Html.text (toString model) ]


(=>) : a -> b -> (a, b)
(=>) = (,)

displayHeight : Height
displayHeight = 600

displayWidth : Width
displayWidth = 480


renderDisplay : Model -> Html Msg
renderDisplay model =
    div
    [ style
        [ "bottom" => "80px"
        , "color" => "#34495f"
        , "font-family" => "Helvetica, Arial, sans-serif"
        , "font-size" => "14px"
        , "left" => "300px"
        , "width" =>  "480px"
        , "padding" => "0px"
        , "position" => "absolute"
        , "right" => "0"
        , "top" => "0"
        , "border" => "1px dashed green"
        ]
    ]
    [ toHtml
        <| container displayWidth displayHeight middle
        <| collage displayWidth displayHeight
        <| [ renderSnake model.snake ]
    ]

skin : Color
skin = rgb 10 120 10

renderSnake : Snake -> Form
renderSnake snake =
        rect 30 20
          |> make snake.head skin


make : Position -> Color -> Shape -> Form
make (x, y) color shape =
    shape
        |> filled color
        |> move (x, y)


-- MAIN

main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
