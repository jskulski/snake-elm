module Main exposing (..)

import Config

-- Domain
import Apple
import Data exposing (Height, Width, Position, Direction(..))


-- Elm Libraries
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.App as Html
import AnimationFrame
import Time exposing (Time)
import Keyboard exposing (..)
import List exposing (take, length, map)
import Color exposing (..)
import Collage exposing (..)
import Element



-- MODEL

type alias Snake =
    { head: Position
    , tail: List Position
    , direction: Direction
    }

initialSnake : Snake
initialSnake = { head = (0, 0)
               , tail = [(0, -Config.cellSize), (0, -Config.cellSize * 2),
                        (0, -Config.cellSize), (0, -Config.cellSize * 2),
                        (0, -Config.cellSize), (0, -Config.cellSize * 2),
                        (0, -Config.cellSize), (0, -Config.cellSize * 2)]
               , direction = North
               }


type GameState = Pre | Playing | Over


type alias Model =
    { snake: Snake
    , gameState: GameState
    , apple: Apple.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { snake = initialSnake
      , gameState = Pre
      , apple = Apple.init
      }
      , Cmd.none )


-- MESSAGES


type Msg
    = TimeUpdate Time
    | StartGame
    | Move Direction
    | EndGame
    | AppleMsg Apple.Msg
    | Noop


-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppleMsg subMsg ->
            let
                (appleModel, appleCmd) = Apple.update subMsg model.apple
            in
                ({ model | apple = appleModel}, Cmd.map AppleMsg appleCmd)

        TimeUpdate dt ->
            (
              { model
              | snake = tickSnake model.snake
              , gameState = hasSnakeDied model.snake Config.displayWidth Config.displayHeight
              }
            , Cmd.none )

        Move direction ->
            ( { model | snake = changeDirection model.snake direction }
            , Cmd.none )

        StartGame ->
            ( { model | gameState = Playing }
            , Cmd.none )

        EndGame ->
            ( { model | gameState = Over }
            , Cmd.none )

        Noop -> ( model, Cmd.none )


changeDirection : Snake -> Direction -> Snake
changeDirection snake newDirection =
    { snake | direction = (sameDirectionIfOpposite newDirection snake.direction) }


sameDirectionIfOpposite : Direction -> Direction -> Direction
sameDirectionIfOpposite new old =
    case new of
        North -> if old == South then South else new
        South -> if old == North then North else new
        East -> if old == West then West else new
        West -> if old == East then East else new


tickSnake : Snake -> Snake
tickSnake snake  =
    let
        move = functionFromDirection snake.direction
    in
        { snake
          | head = move snake.head
          , tail = (snake.head :: allButLast snake.tail)
        }

allButLast : List a -> List a
allButLast tail =
    take (length tail - 1) tail

-- The game is over when the snake hits a wall
hasSnakeDied : Snake -> Width -> Height -> GameState
hasSnakeDied { head, tail, direction } displayWidth displayHeight =
    let
        (x, y) = head
        collisionBuffer = -Config.cellSize / 2
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
    (x, y + Config.velocity)


south : Position -> Position
south (x, y) =
    (x, y - Config.velocity)


east : Position -> Position
east (x, y) =
    (x + Config.velocity, y)


west : Position -> Position
west (x, y) =
    (x - Config.velocity, y)

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
    [ Element.toHtml
        <| Element.container Config.displayWidth Config.displayHeight Element.middle
        <| collage Config.displayWidth Config.displayHeight
        <| Apple.render model.apple :: renderSnake model.snake
    ]

skin : Color
skin = rgb 10 120 10

lighterSkin : Color
lighterSkin = rgb 20 160 20


renderHead : Position -> Form
renderHead (x, y) =
    rect Config.cellSize Config.cellSize
    |> filled skin
    |> move (x, y)


renderTail : List Position -> List Form
renderTail tail =
    List.map renderTailPart tail


renderTailPart : Position -> Form
renderTailPart (x, y) =
    rect Config.cellSize Config.cellSize
    |> filled skin
    |> move (x, y)


renderSnake : Snake -> List Form
renderSnake snake =
    renderHead snake.head :: renderTail snake.tail


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
