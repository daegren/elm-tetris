module Tetromino
    exposing
        ( Direction
        , Rotation(..)
        , Shape(..)
        , Tetromino
        , cell
        , color
        , init
        , moveDown
        , moveLeft
        , moveRight
        , points
        , position
        , rotate
        , view
        )

import Collage
import Color
import Point exposing (Point)


type Tetromino
    = Tetromino
        { shape : Shape
        , position : Point
        , direction : Direction
        }


type Shape
    = O
    | T
    | I
    | S
    | Z
    | J
    | L


type Direction
    = Up
    | Right
    | Down
    | Left


type Rotation
    = Clockwise
    | CounterClockwise


init : Shape -> Tetromino
init shape =
    Tetromino
        { shape = shape
        , direction = Up
        , position = spawnPosition
        }


spawnPosition : Point
spawnPosition =
    ( -1, 9 )


position : Tetromino -> Point
position (Tetromino { position }) =
    position


cellsForShape : Shape -> List Point
cellsForShape shape =
    case shape of
        O ->
            [ ( -1, 0 ), ( 0, 0 ), ( 0, -1 ), ( -1, -1 ) ]

        T ->
            [ ( -1, 0 ), ( 0, 0 ), ( 0, 1 ), ( 1, 0 ) ]

        I ->
            [ ( -2, 0 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]

        S ->
            [ ( -1, 0 ), ( 0, 0 ), ( 0, 1 ), ( 1, 1 ) ]

        Z ->
            [ ( -1, 1 ), ( 0, 1 ), ( 0, 0 ), ( 1, 0 ) ]

        J ->
            [ ( -1, 1 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]

        L ->
            [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 1, 1 ) ]


colorForShape : Shape -> Color.Color
colorForShape shape =
    case shape of
        O ->
            Color.rgb 255 255 0

        T ->
            Color.rgb 255 0 255

        I ->
            Color.rgb 0 255 255

        S ->
            Color.rgb 0 255 0

        Z ->
            Color.rgb 255 0 0

        J ->
            Color.rgb 0 0 255

        L ->
            Color.rgb 255 165 0


color : Tetromino -> Color.Color
color (Tetromino { shape }) =
    colorForShape shape



-- HELPERS


moveDown : Tetromino -> Tetromino
moveDown ((Tetromino tetromino) as t) =
    Tetromino { tetromino | position = Point.moveDown (position t) }


moveLeft : Tetromino -> Tetromino
moveLeft ((Tetromino tetromino) as t) =
    Tetromino { tetromino | position = Point.moveLeft (position t) }


moveRight : Tetromino -> Tetromino
moveRight ((Tetromino tetromino) as t) =
    Tetromino { tetromino | position = Point.moveRight (position t) }


{-| Gets all the points that represent a tetromino, accounting for current direction
-}
points : Tetromino -> List Point
points (Tetromino { direction, shape }) =
    rotatePoints direction shape


{-| Rotate the direction of a tetromino in the given rotation
-}
rotate : Rotation -> Tetromino -> Tetromino
rotate rotation (Tetromino tetromino) =
    Tetromino
        { tetromino
            | direction =
                case rotation of
                    Clockwise ->
                        case tetromino.direction of
                            Up ->
                                Right

                            Right ->
                                Down

                            Down ->
                                Left

                            Left ->
                                Up

                    CounterClockwise ->
                        case tetromino.direction of
                            Up ->
                                Left

                            Left ->
                                Down

                            Down ->
                                Right

                            Right ->
                                Up
        }


rotatePoints : Direction -> Shape -> List Point
rotatePoints direction shape =
    let
        points =
            cellsForShape shape

        numOfTurns =
            case direction of
                Up ->
                    0

                Right ->
                    1

                Down ->
                    2

                Left ->
                    3

        offset =
            case shape of
                I ->
                    ( 0.5, 0.5 )

                O ->
                    ( 0.5, 0.5 )

                _ ->
                    ( 0, 0 )

        negativeOffset =
            mapTuple (\a b -> ( -1 * a, -1 * b )) offset
    in
    if direction == Up then
        points
    else
        List.map (Point.add offset) points
            |> List.map toPolar
            |> List.map (Tuple.mapSecond (\a -> a - turns (0.25 * numOfTurns)))
            |> List.map fromPolar
            |> List.map (Point.add negativeOffset)
            |> List.map (\( x, y ) -> ( fixRoundingErrors x, fixRoundingErrors y ))


mapTuple : (a -> b -> ( a1, b1 )) -> ( a, b ) -> ( a1, b1 )
mapTuple mapper ( a, b ) =
    mapper a b


fixRoundingErrors : Float -> Float
fixRoundingErrors a =
    round a |> toFloat



-- VIEW


view : Float -> Tetromino -> Collage.Form
view cellSize (Tetromino { direction, shape }) =
    rotatePoints direction shape
        |> List.map
            (\( x, y ) ->
                cell cellSize (colorForShape shape)
                    |> Collage.move ( cellSize * x, cellSize * y )
            )
        |> Collage.group


cell : Float -> Color.Color -> Collage.Form
cell size color =
    let
        half =
            size / 2

        embossWidth =
            size * 0.125
    in
    Collage.group
        [ Collage.rect size size
            |> Collage.filled color
        , Collage.polygon [ ( -half, -half ), ( -half, half ), ( half, half ), ( half - embossWidth, half - embossWidth ), ( -half + embossWidth, half - embossWidth ), ( -half + embossWidth, -half + embossWidth ) ]
            |> Collage.filled (Color.rgba 255 255 255 0.5)
        , Collage.polygon [ ( -half, -half ), ( half, -half ), ( half, half ), ( half - embossWidth, half - embossWidth ), ( half - embossWidth, -half + embossWidth ), ( -half + embossWidth, -half + embossWidth ) ]
            |> Collage.filled (Color.rgba 0 0 0 0.5)
        , Collage.segment ( -half, half ) ( -half + embossWidth, half - embossWidth )
            |> Collage.traced (Collage.solid (Color.rgba 0 0 0 0.125))
        , Collage.segment ( half, -half ) ( half - embossWidth, -half + embossWidth )
            |> Collage.traced (Collage.solid (Color.rgba 255 255 255 0.125))
        , Collage.path [ ( -half + embossWidth, -half + embossWidth ), ( -half + embossWidth, half - embossWidth ), ( half - embossWidth, half - embossWidth ), ( half - embossWidth, -half + embossWidth ) ]
            |> Collage.traced (Collage.solid (Color.rgba 0 0 0 0.125))
        ]
