module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (div, img, text)
import Html.Attributes exposing (src, style)
import List exposing (length, map)
import Random
import Time


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


dropFrequency =
    100


subscriptions drops =
    if length drops < numDrops then
        Sub.batch
            [ onAnimationFrameDelta Tick
            , Time.every dropFrequency <| always GenerateDrop
            ]

    else
        onAnimationFrameDelta Tick


type alias Drop =
    ( Float, Float )


type alias Milliseconds =
    Float


type Msg
    = Tick Milliseconds
    | AddDrop Drop
    | GenerateDrop


generateDrop =
    Random.generate (\x -> AddDrop ( toFloat x, 0 )) (Random.int 0 width)


init () =
    ( [ ( 50.0, 50.0 )
      , ( 100.0, 250.0 )
      ]
    , generateDrop
    )


width =
    1409


height =
    641


numDrops =
    200


viewDrop ( x, y ) =
    div
        [ style "position" "absolute"
        , style "left" (String.fromFloat x ++ "px")
        , style "top" (String.fromFloat y ++ "px")
        , style "width" "2px"
        , style "height" "20px"
        , style "background-color" "blue"
        ]
        []


view drops =
    div
        []
        ([ text <| "Drop count: " ++ String.fromInt (length drops)
         , img
            [ src "bg.png"
            , style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            , style "z-index" "-1"
            ]
            []
         ]
            ++ map viewDrop drops
        )


wrap a by =
    if a > by then
        a - by

    else
        a


updateDrop delta ( x, y ) =
    ( x, wrap (y + delta) height )


update msg model =
    case msg of
        Tick delta ->
            ( map (updateDrop delta) model, Cmd.none )

        AddDrop drop ->
            ( drop :: model
            , Cmd.none
            )

        GenerateDrop ->
            ( model, generateDrop )
