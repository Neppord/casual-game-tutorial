module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Html exposing (div, text)
import Html.Attributes
import Json.Decode as Decode
import Svg exposing (g, image, svg)
import Svg.Attributes
import TypedSvg.Attributes exposing (height, width)
import TypedSvg.Types exposing (px)



-- top level functions


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


init () =
    ( StartScreen, Cmd.none )


view model =
    case model of
        StartScreen ->
            text "PRESS S TO SIMULATE"

        SimScreen { x, y, r } ->
            let
                radiusS =
                    String.fromFloat r

                diameter =
                    r * 2

                rotationS =
                    String.fromFloat x
            in
            div
                [ Html.Attributes.style "user-select" "none"
                , Html.Attributes.style "color" "darkgreen"
                , Html.Attributes.style "font-size" "21px"
                , Html.Attributes.style "font-family" "Arial"
                ]
                [ svg
                    [ width <| px canvasWidth
                    , height <| px canvasHeight
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
                            , width <| px diameter
                            , height <| px diameter
                            , Svg.Attributes.transform ("rotate(" ++ rotationS ++ ", " ++ radiusS ++ ", " ++ radiusS ++ ")")
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
                , r = ballRadius
                , dx = -0.1
                , dy = 0.1
                }
            , Cmd.none
            )

        ( SimScreen ({ y, dy } as ball), Tick delta ) ->
            ( if stopCondition y dy then
                StartScreen

              else
                SimScreen (updateBall delta ball)
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


subscriptions model =
    case model of
        StartScreen ->
            onKeyDown <| Decode.map keyDown decodeKey

        SimScreen _ ->
            onAnimationFrameDelta Tick



-- update helper functions


stopCondition y dy =
    abs (y + ballRadius - canvasHeight) < 1 && dy < 0.01


updateBall delta ball =
    let
        project =
            projectBall delta

        projectedball =
            project ball

        hc =
            hCollision projectedball

        vc =
            vCollision projectedball
    in
    project
        { ball
            | dx = nextDelta hc ball.dx
            , dy = nextDelta vc ball.dy + gravitation
        }


projectBall delta ({ x, dx, y, dy } as model) =
    { model | x = x + delta * dx, y = y + delta * dy }


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


type alias Milliseconds =
    Float



-- constants


canvasWidth =
    640


canvasHeight =
    480


gravitation =
    0.1


ballRadius =
    50.0
