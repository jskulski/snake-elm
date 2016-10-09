module Apple exposing (Msg(..), Model, update, init, render)

-- Domain
import Config exposing (displayWidth, displayHeight)
import Data exposing (Position)
import Random exposing (Generator, generate, pair, float)

-- Elm
import Maybe exposing (Maybe(Nothing))
import Collage
import Color


type alias Model =
    { position: Maybe Position
    }

init : Model
init = { position = Nothing }

type Msg
    = Growth Position
    | TimeForGrowth


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Growth (x, y) ->
            ({ model | position = Just (x, y) }, Cmd.none)


        TimeForGrowth ->
            (model, generate Growth randomPosition)



randomPosition : Generator (Float, Float)
randomPosition =
    pair (float 0 (toFloat Config.displayWidth)) (float 0 (toFloat Config.displayHeight))


-- RENDER

appleColor = Color.rgb 180 10 10

-- TODO: how do i deal with the maybe here? Maybe Collage.Form?
render : Model -> Collage.Form
render { position }=
    case position of
        Nothing -> Collage.circle 1 |> Collage.filled appleColor
        Just position -> Collage.circle Config.appleSize
                         |> Collage.filled appleColor
                         |> Collage.move (position)

