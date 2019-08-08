module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onClick)
import Json.Decode as Decode
import Random
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, style, width)


canvasWidth =
    640


canvasHeight =
    480


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions _ =
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


type alias Ball =
    { x : Float, y : Float, r : Float, dx : Float, dy : Float }


generateBall : Float -> Random.Generator Ball
generateBall r =
    let
        genX =
            Random.float r (canvasHeight - r)

        genY =
            Random.float r (canvasWidth - r)

        genD =
            Random.float -1.0 1.0

        ball x y dx dy =
            { x = x, y = y, r = r, dx = dx, dy = dy }
    in
    Random.map4 ball genX genY genD genD


init () =
    ( { x = 200.0
      , y = 200.0
      , r = 200.0
      , dx = 1.0
      , dy = 1.0
      }
    , Cmd.none
    )


view { x, y, r } =
    svg
        [ width <| String.fromInt canvasWidth
        , height <| String.fromInt canvasHeight
        , style "position: absolute; top: 0; left: 0; background-color: gray;"
        ]
        [ circle
            [ fill "red"
            , cx <| String.fromFloat x
            , cy <| String.fromFloat y
            , Svg.Attributes.r <| String.fromFloat r
            ]
            []
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
            x - ball.x

        b =
            y - ball.y

        c =
            ball.r
    in
    (a ^ 2) + (b ^ 2) < (c ^ 2)


update msg model =
    case msg of
        Tick delta ->
            ( updateBall delta model, Cmd.none )

        Shot x y ->
            if withInBall model x y then
                ( model, Random.generate NewBall <| generateBall (model.r * 0.95) )

            else
                ( model, Cmd.none )

        NewBall ball ->
            ( ball, Cmd.none )


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
