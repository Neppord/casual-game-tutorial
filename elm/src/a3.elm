module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (div)
import Html.Attributes exposing (style)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions _ =
    onAnimationFrameDelta identity


init () =
    ( { x = 0.0, dx = 1 }, Cmd.none )


view { x } =
    div
        [ style "position" "absolute"
        , style "left" (String.fromFloat x ++ "px")
        , style "background-color" "lightblue"
        , style "width" "50px"
        , style "height" "50px"
        ]
        []


playSize =
    500


withinPlayField x =
    0 < x && x < playSize


update delta ({ x, dx } as model) =
    let
        nextX =
            x + (delta * dx)

        nextDx =
            if withinPlayField nextX then
                dx

            else
                -dx
    in
    ( { model | x = x + (delta * nextDx), dx = nextDx }, Cmd.none )
