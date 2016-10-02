module Main exposing (..)

import Maybe exposing (Maybe)
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

type alias Position = (Float, Float)


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


type State = Pre | Playing | Over


type alias Model =
    { snake: Snake
    , state: State
    }


init : ( Model, Cmd Msg )
init =
    ( { snake = { head = (0, 0)
                , tail = [ (0, 1) ]
                , direction = North
                }
      , state = Pre
      }
      , Cmd.none )


-- MESSAGES


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode


-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( { snake = moveOnce model.snake
              , state = Playing
              }
            , Cmd.none )

        KeyDown keycode ->
            ( { snake = changeDirection model.snake (direction keycode)
              , state = Playing
              } , Cmd.none)



direction : Int -> Maybe Direction
direction keycode =
    case keycode of
        87 -> Just North
        83 -> Just South
        65 -> Just West
        68 -> Just East
        _ -> Nothing



changeDirection : Snake -> Maybe Direction -> Snake
changeDirection { head, tail, direction } maybeDirection =
    case maybeDirection of
        Nothing -> { head = head , tail = tail, direction = direction }
        Just newDirection -> { head = head
                             , tail = tail
                             , direction = newDirection
                             }


moveOnce : Snake -> Snake
moveOnce { head, tail, direction } =
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
        [ if model.state == Playing then
            AnimationFrame.diffs TimeUpdate
          else
            Sub.none
        , Keyboard.downs KeyDown
        ]

-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ renderState model
        , renderBoard model
        ]


renderState : Model -> Html Msg
renderState model =
    div [ style
            [ "position" => "absolute"
            , "bottom" => "0"
            , "left" => "0"
            ]
        ]
        [ Html.text (toString model) ]


(=>) : a -> b -> (a, b)
(=>) = (,)

renderBoard : Model -> Html Msg
renderBoard model =
    div
    [ style
        [ "bottom" => "80px"
        , "color" => "#34495f"
        , "font-family" => "Helvetica, Arial, sans-serif"
        , "font-size" => "14px"
        , "left" => "300px"
        , "padding" => "0 30px"
        , "position" => "absolute"
        , "right" => "0"
        , "top" => "0"
        ]
    ]
    [ toHtml
        <| container 600 480 middle
        <| collage 600 480
        <| [ renderSnake model.snake ]
    ]

skin = rgb 10 120 10

renderSnake : Snake -> Form
renderSnake snake =
        circle 30
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
