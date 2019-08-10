module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown)
import Html exposing (div, text)
import Html.Attributes
import Json.Decode as Decode
import Random
import Svg exposing (g, image, svg)
import Svg.Attributes



-- top level functions


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init () =
    ( StartScreen, Cmd.none )


view model =
    case model of
        StartScreen ->
            text "PRESS S TO START"

        SimScreen { x, y, r } ->
            let
                raddie =
                    String.fromFloat r
            in
            div
                [ Html.Attributes.style "user-select" "none"
                , Html.Attributes.style "color" "darkgreen"
                , Html.Attributes.style "font-size" "21px"
                , Html.Attributes.style "font-family" "Arial"
                ]
                [ svg
                    [ Svg.Attributes.width <| String.fromInt canvasWidth
                    , Svg.Attributes.height <| String.fromInt canvasHeight
                    , Svg.Attributes.style "position: absolute; top: 0; left: 0; background-color: lightblue; z-index: -1"
                    ]
                    [ g
                        [ Svg.Attributes.transform
                            ("translate("
                                ++ String.fromFloat (x - r)
                                ++ ","
                                ++ String.fromFloat (y - r)
                                ++ ")"
                            )
                        ]
                        [ image
                            [ Svg.Attributes.xlinkHref "ball.png"
                            , Svg.Attributes.width <| String.fromFloat (r * 2)
                            , Svg.Attributes.height <| String.fromFloat (r * 2)
                            , Svg.Attributes.transform ("rotate(180, " ++ raddie ++ ", " ++ raddie ++ ")")
                            ]
                            []
                        ]
                    ]
                ]


update msg model =
    case ( model, msg ) of
        ( StartScreen, StartSimulation ) ->
            ( SimScreen
                { x = 200.0
                , y = 200.0
                , r = 50.0
                , dx = 1.0
                , dy = 1.0
                }
            , Cmd.none
            )

        ( SimScreen ball, Tick delta ) ->
            ( SimScreen (updateBall delta ball), Cmd.none )

        ( SimScreen _, StartSimulation ) ->
            ( StartScreen, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions model =
    case model of
        StartScreen ->
            onKeyDown <| Decode.map keyDown decodeKey

        SimScreen _ ->
            onAnimationFrameDelta Tick



-- update helper functions


updateBall delta model =
    let
        project =
            projectModel delta

        projectedModel =
            project model

        hc =
            hCollision projectedModel

        vc =
            vCollision projectedModel
    in
    project
        { model
            | dx = nextDelta hc model.dx
            , dy = nextDelta vc model.dy
        }


projectModel delta ({ x, dx, y, dy } as model) =
    { model | x = x + delta * 0.1 * dx, y = y + delta * 0.1 * dy }


hCollision { x, r } =
    rangeCompare { start = r, stop = canvasWidth - r } x


vCollision { y, r } =
    rangeCompare { start = r, stop = canvasHeight - r } y


rangeCompare { start, stop } value =
    case ( compare value start, compare value stop ) of
        ( LT, _ ) ->
            LT

        ( _, GT ) ->
            GT

        ( _, _ ) ->
            EQ


nextDelta c d =
    case c of
        LT ->
            abs d

        GT ->
            negate <| abs d

        EQ ->
            d



-- subscription helper functions


decodeKey =
    Decode.field "key" Decode.string


keyDown letter =
    case letter of
        "s" ->
            StartSimulation

        _ ->
            Nop



-- types


type Model
    = StartScreen
    | SimScreen Ball


type Msg
    = Tick Milliseconds
    | StartSimulation
    | Nop


type alias Ball =
    { x : Float, y : Float, r : Float, dx : Float, dy : Float }


type alias Collision =
    ( Order, Order )


type alias Milliseconds =
    Float



-- constants


canvasWidth =
    640


canvasHeight =
    480


speed =
    10.0
