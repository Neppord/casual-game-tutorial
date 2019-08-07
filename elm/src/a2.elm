module Main exposing (main)

import Browser
import Browser.Events as Sub
import Html exposing (audio, div, text)
import Html.Attributes exposing (autoplay, preload, src)
import Json.Decode as Decode


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions _ =
    let
        decodeKey =
            Decode.field "key" Decode.string

        keyDown _ =
            DDown

        keyUp _ =
            DUp
    in
    Sub.batch
        [ Sub.onKeyDown <| Decode.map keyDown <| decodeKey
        , Sub.onKeyUp <| Decode.map keyUp <| decodeKey
        ]


init () =
    ( { d = False }, Cmd.none )


type Msg
    = DDown
    | DUp


dview =
    div []
        [ audio [ src "d.wav", autoplay True, preload "auto" ] []
        , text "d"
        ]


view { d } =
    div []
        (if d then
            [ dview ]

         else
            []
        )


update msg model =
    case msg of
        DDown ->
            ( { model | d = True }, Cmd.none )

        DUp ->
            ( { model | d = False }, Cmd.none )
