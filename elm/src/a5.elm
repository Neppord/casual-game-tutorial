module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, width)


canvasWidth =
    640


canvasHeight =
    480


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions _ =
    onAnimationFrameDelta Tick


type alias Milliseconds =
    Float


type Msg
    = Tick Milliseconds


init () =
    ( { x = 50.0
      , y = 50.0
      , r = 50.0
      , dx = 1.0
      , dy = 1.0
      }
    , Cmd.none
    )


view { x, y, r } =
    svg
        [ width <| String.fromInt canvasWidth, height <| String.fromInt canvasHeight ]
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
    { model | x = x + delta * dx, y = y + delta * dy }


nextDelta c d =
    case c of
        LT ->
            1.0

        GT ->
            -1.0

        EQ ->
            d


update msg model =
    case msg of
        Tick delta ->
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
            ( project
                { model
                    | dx = nextDelta hc model.dx
                    , dy = nextDelta vc model.dy
                }
            , Cmd.none
            )
