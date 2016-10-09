module Apple exposing (Msg(..), Model, update, init)

-- Domain
import Config exposing (displayWidth, displayHeight)
import Data exposing (Position)
import Random exposing (Generator, pair, float)

-- Elm
import Maybe exposing (Maybe(Nothing))


type alias Model =
    { position: Maybe Position
    }

init : Model
init = { position = Nothing }

type Msg = Growth Position


update : Msg -> Model -> Model
update msg model =
    case msg of
        Growth (x, y) -> { model | position = Just (x, y) }



randomPosition : Generator (Float, Float)
randomPosition =
    pair (float 0 (toFloat Config.displayWidth)) (float 0 (toFloat Config.displayHeight))