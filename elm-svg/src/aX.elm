module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (div, text)


width =
    640


height =
    480


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions _ =
    onAnimationFrameDelta Tick


type alias Milliseconds =
    Float


type Msg
    = Tick Milliseconds


init () =
    ( (), Cmd.none )


view _ =
    div
        []
        [ text "Challenge" ]


update msg model =
    case msg of
        Tick _ ->
            ( model, Cmd.none )
