module Main exposing (main)

import Browser
import Html exposing (div, input, text)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput)


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
            case String.toInt s of
                Just x ->
                    ( { model | x = x }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        YChange s ->
            case String.toInt s of
                Just y ->
                    ( { model | y = y }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )
