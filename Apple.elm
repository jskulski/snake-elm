module Apple exposing (Msg(..), Model, update, init, render)

-- Domain
import Config exposing (displayWidth, displayHeight)
import World exposing (Cell)
import Random exposing (Generator, generate, pair, float)

-- Elm
import Maybe exposing (Maybe(Nothing))
import Collage
import Color


type alias Model =
    { position: Maybe Cell
    }

init : Model
-- init = { position = Nothing }
init = { position = Just (100, 100)}

type Msg
    = Growth Cell
    | TimeForGrowth
    | Eaten



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Growth (x, y) ->
            ({ model | position = Just (x, y) }, Cmd.none)

        TimeForGrowth ->
            (model, generate Growth randomPosition)

        Eaten ->
            ({model | position = Nothing}, Cmd.none)



randomPosition : Generator (Float, Float)
randomPosition =
    pair (float 0 (toFloat Config.displayWidth)) (float 0 (toFloat Config.displayHeight))


-- RENDER

appleColor = Color.rgb 180 10 10

-- TODO: how do i deal with the maybe here? Maybe Collage.Form?
render : Model -> List Collage.Form
render { position }=
    case position of
        Nothing -> []
        Just position -> [ Collage.circle Config.appleSize
                           |> Collage.filled appleColor
                           |> Collage.move (position)
                         ]


