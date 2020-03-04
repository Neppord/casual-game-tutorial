module Main exposing (main)

import Browser
import Browser.Events as Sub
import Html exposing (audio, div, text)
import Html.Attributes exposing (autoplay, preload, src, style)
import Json.Decode as Decode


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


decodeKey =
    Decode.field "key" Decode.string


keyDown letter =
    case letter of
        "a" ->
            ADown

        "s" ->
            SDown

        "d" ->
            DDown

        _ ->
            DDown


keyUp letter =
    case letter of
        "a" ->
            ARelease

        "s" ->
            SRelease

        "d" ->
            DRelease

        _ ->
            DRelease


subscriptions _ =
    Sub.batch
        [ Sub.onKeyDown <| Decode.map keyDown decodeKey
        , Sub.onKeyUp <| Decode.map keyUp decodeKey
        ]


init () =
    ( { a = False, s = False, d = False }, Cmd.none )


type Msg
    = ADown
    | ARelease
    | SDown
    | SRelease
    | DDown
    | DRelease


styleSpan =
    div [ style "margin" "50px", style "width" "2em", style "float" "left" ]


drumView l =
    styleSpan
        [ audio [ src (l ++ ".wav"), autoplay True, preload "auto" ] []
        , text l
        ]


view { a, s, d } =
    let
        empty =
            [ styleSpan [] ]

        avar =
            if a then
                [ drumView "a" ]

            else
                empty

        svar =
            if s then
                [ drumView "s" ]

            else
                empty

        dvar =
            if d then
                [ drumView "d" ]

            else
                empty
    in
    div []
        (avar
            ++ svar
            ++ dvar
        )


update msg model =
    case msg of
        ADown ->
            ( { model | a = True }, Cmd.none )

        ARelease ->
            ( { model | a = False }, Cmd.none )

        SDown ->
            ( { model | s = True }, Cmd.none )

        SRelease ->
            ( { model | s = False }, Cmd.none )

        DDown ->
            ( { model | d = True }, Cmd.none )

        DRelease ->
            ( { model | d = False }, Cmd.none )
