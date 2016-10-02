module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App as Html
import AnimationFrame
import Time exposing (Time)
import Keyboard exposing (..)




-- MODEL

type alias Position = (Int, Int)


type Direction
 = North
 | East
 | South
 | West


type alias Point = (Int, Int)


type alias P = Point


type alias Snake =
    { head: Position
    , tail: List Position
    , direction: Direction
    }


type alias Apple =
    { position: Position
    }


type alias Model =
    { snake: Snake
    }


init : ( Model, Cmd Msg )
init =
    ( { snake = { head = (0, 0)
                , tail = [ (0, 1) ]
                , direction = North
                }
      }
      , Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text (toString model) ]

-- MESSAGES


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode


-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( { snake = moveOnce model.snake }, Cmd.none )

        KeyDown keycode ->
            ( { snake = changeDirection model.snake (direction keycode) } , Cmd.none)

        KeyUp keycode ->
            ( model, Cmd.none)



direction : Int -> Direction
direction keycode =
    case keycode of
        87 -> North
        83 -> South
        65 -> West
        68 -> East
        _ -> North



changeDirection : Snake -> Direction -> Snake
changeDirection { head, tail, direction } newDirection =
    { head = head
    , tail = tail
    , direction = newDirection
    }


moveOnce : Snake -> Snake
moveOnce { head, tail , direction } =
    let
        moveFunction = functionFromDirection direction
    in
        { head = moveFunction head
        , tail = List.map moveFunction tail
        , direction = direction
        }


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
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]

-- MAIN

main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
