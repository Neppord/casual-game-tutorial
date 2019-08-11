module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick)
import Html exposing (div, text)
import Html.Attributes
import Json.Decode as Decode
import Random
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, width)


canvasWidth =
    640


canvasHeight =
    480


speed =
    10.0


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions ( _, ball ) =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onClick (Decode.map2 (Shot ball) (Decode.field "x" Decode.float) (Decode.field "y" Decode.float))
        ]


type alias Milliseconds =
    Float


type Msg
    = Tick Milliseconds
    | Shot Ball Float Float
    | NewBall Ball


type alias Ball =
    { x : Float, y : Float, r : Float, dx : Float, dy : Float }


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
    ( ( 0
      , { x = 200.0
        , y = 200.0
        , r = 200.0
        , dx = 1.0
        , dy = 1.0
        }
      )
    , Cmd.none
    )


view ( score, { x, y, r } ) =
    div
        [ Html.Attributes.style "user-select" "none"
        , Html.Attributes.style "color" "darkgreen"
        , Html.Attributes.style "font-size" "21px"
        , Html.Attributes.style "font-family" "Arial"
        ]
        [ text ("Score: " ++ String.fromInt score)
        , svg
            [ width <| String.fromInt canvasWidth
            , height <| String.fromInt canvasHeight
            , Svg.Attributes.style "position: absolute; top: 0; left: 0; background-color: lightblue; z-index: -1"
            ]
            [ circle
                [ fill "red"
                , cx <| String.fromFloat x
                , cy <| String.fromFloat y
                , Svg.Attributes.r <| String.fromFloat r
                ]
                []
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


withInBall ball x y =
    let
        a =
            abs (x - ball.x)

        b =
            abs (y - ball.y)

        c =
            sqrt ((a ^ 2) + (b ^ 2))
    in
    c < ball.r


update msg ( score, ball ) =
    case msg of
        Tick delta ->
            ( ( score, updateBall delta ball ), Cmd.none )

        Shot shotBall x y ->
            ( ( score, ball )
            , if withInBall shotBall x y then
                Random.generate NewBall <| generateBall (ball.r * 0.95)

              else
                Cmd.none
            )

        NewBall newBall ->
            ( ( score + 1, newBall ), Cmd.none )


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
