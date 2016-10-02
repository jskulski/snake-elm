module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App


-- MODEL

type alias Position = (Int, Int)

-- type Direction
--  = North
--  | East
--  | South
--  | West

-- type alias Snake =
--     { head: Position
--     , tail: List Position
--     , direction: Direction
--     }

type alias Apple =
    { position: Position
    }


type Model =
    NotStarted | Started Position


init : ( Model, Cmd Msg )
init =
    ( NotStarted, Cmd.none )

-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text "Game not started" ]

-- MESSAGES


type Msg =
    Tick


-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- MAIN

main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
