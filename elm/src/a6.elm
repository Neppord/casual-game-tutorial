module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown)
import Html exposing (div, text)
import Html.Attributes
import Json.Decode as Decode
import Random
import Svg exposing (g, image, svg)
import Svg.Attributes


canvasWidth =
    640


canvasHeight =
    480


speed =
    10.0


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


decodeKey =
    Decode.field "key" Decode.string


keyDown letter =
    case letter of
        "s" ->
            SwitchScreen

        _ ->
            Nop


subscriptions model =
    case model of
        StartScreen ->
            onKeyDown <| Decode.map keyDown decodeKey

        SimScreen _ ->
            Sub.batch
                [ onAnimationFrameDelta Tick
                , onClick (Decode.map2 Shot (Decode.field "x" Decode.float) (Decode.field "y" Decode.float))
                ]


type alias Milliseconds =
    Float


type Msg
    = Tick Milliseconds
    | Shot Float Float
    | NewBall Ball
    | SwitchScreen
    | Nop


type alias Score =
    Int


type alias Ball =
    { x : Float, y : Float, r : Float, dx : Float, dy : Float }


type Model
    = StartScreen
    | SimScreen ( Score, Ball )


generateBall : Float -> Random.Generator Ball
generateBall r =
    let
        genX =
            Random.float r (canvasWidth - r)

        genY =
            Random.float r (canvasHeight - r)

        genD =
            Random.float (negate speed) speed

        ball x y dx dy =
            { x = x, y = y, r = r, dx = dx, dy = dy }
    in
    Random.map4 ball genX genY genD genD


init () =
    ( StartScreen, Cmd.none )


view model =
    case model of
        StartScreen ->
            text "PRESS S TO START"

        SimScreen ( score, { x, y, r } ) ->
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
                [ text ("Score: " ++ String.fromInt score)
                , svg
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


type alias Collision =
    ( Order, Order )


rangeEqual { start, stop } value =
    case ( compare value start, compare value stop ) of
        ( LT, _ ) ->
            LT

        ( _, GT ) ->
            GT

        ( _, _ ) ->
            EQ


hCollision { x, r } =
    rangeEqual { start = r, stop = canvasWidth - r } x


vCollision { y, r } =
    rangeEqual { start = r, stop = canvasHeight - r } y


projectModel delta ({ x, dx, y, dy } as model) =
    { model | x = x + delta * 0.1 * dx, y = y + delta * 0.1 * dy }


nextDelta c d =
    case c of
        LT ->
            abs d

        GT ->
            negate <| abs d

        EQ ->
            d


withinBall ( x, y ) ball =
    let
        a =
            abs (x - ball.x)

        b =
            abs (y - ball.y)

        c =
            sqrt ((a ^ 2) + (b ^ 2))
    in
    c < ball.r


update msg model =
    case ( model, msg ) of
        ( StartScreen, SwitchScreen ) ->
            ( SimScreen
                ( 0
                , { x = 200.0
                  , y = 200.0
                  , r = 200.0
                  , dx = 1.0
                  , dy = 1.0
                  }
                )
            , Cmd.none
            )

        ( SimScreen ( score, ball ), Tick delta ) ->
            ( SimScreen ( score, updateBall delta ball ), Cmd.none )

        ( SimScreen ( score, ball ), Shot x y ) ->
            ( SimScreen ( score, ball )
            , if withinBall ( x, y ) ball then
                Random.generate NewBall <| generateBall (ball.r * 0.95)

              else
                Cmd.none
            )

        ( SimScreen ( score, _ ), NewBall newBall ) ->
            ( SimScreen ( score + 1, newBall ), Cmd.none )

        ( SimScreen _, SwitchScreen ) ->
            ( StartScreen, Cmd.none )

        _ ->
            ( model, Cmd.none )


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
