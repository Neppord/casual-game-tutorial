module Main exposing (main)

import Browser
import Html exposing (div, input, text)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput)
import Maybe exposing (withDefault)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { x : Int, y : Int }


subscriptions _ =
    Sub.none


init () =
    ( { x = 0, y = 0 }, Cmd.none )


type Msg
    = XChange String
    | YChange String


view { x, y } =
    div []
        [ input [ placeholder "x", onInput XChange ] []
        , input [ placeholder "y", onInput YChange ] []
        , div
            [ style "position" "relative"
            , style "left" (String.fromInt x ++ "px")
            , style "top" (String.fromInt y ++ "px")
            ]
            [ text "Hello World!" ]
        ]


update msg model =
    case msg of
        XChange s ->
            ( { model | x = withDefault model.x (String.toInt s) }, Cmd.none )

        YChange s ->
            ( { model | y = withDefault model.y (String.toInt s) }, Cmd.none )
