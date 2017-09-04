module Game exposing (..)

import Collage
import Color
import Element
import Html exposing (Html)


-- MODEL


type alias Game =
    { current : Tetromino
    , level : Level
    , interval : Float
    }


type alias Tetromino =
    { shape : Shape
    , position : ( Int, Int )
    }


type alias Level =
    Int


type Shape
    = Circle


initialGame : Game
initialGame =
    { current =
        { shape = Circle
        , position = ( -1, 9 )
        }
    , level = 1
    , interval = 0
    }



-- UPDATE


speedForLevel : Level -> Float
speedForLevel level =
    1000


tickGame : Float -> Game -> Game
tickGame delta game =
    let
        currentInterval =
            game.interval + delta

        ( current, interval ) =
            if currentInterval > speedForLevel game.level then
                ( tickCurrent game.current, currentInterval - speedForLevel game.level )
            else
                ( game.current, currentInterval )
    in
    { game | interval = interval, current = current }


tickCurrent : Tetromino -> Tetromino
tickCurrent tetromino =
    let
        position =
            tetromino.position
                |> Tuple.mapSecond
                    (\y ->
                        if y > -10 then
                            y - 1
                        else
                            y
                    )
    in
    { tetromino | position = position }



-- VIEW


view : Game -> Html msg
view game =
    let
        height =
            20 * cellSize

        width =
            10 * cellSize
    in
    Element.toHtml <|
        Collage.collage width
            height
            [ backgroundView width height
            , currentView game
            ]


cellSize : number
cellSize =
    40


backgroundColor : Color.Color
backgroundColor =
    Color.rgb 32 32 32


backgroundView : Float -> Float -> Collage.Form
backgroundView width height =
    Collage.rect width height
        |> Collage.filled backgroundColor


currentView : Game -> Collage.Form
currentView game =
    let
        ( posX, posY ) =
            game.current.position

        x =
            toFloat (posX * cellSize) + (cellSize / 2)

        y =
            toFloat (posY * cellSize) + (cellSize / 2)
    in
    tetrominoCell game.current
        |> Collage.move ( x, y )


tetrominoCell : Tetromino -> Collage.Form
tetrominoCell tetromino =
    let
        half =
            cellSize / 2

        embossWidth =
            cellSize * 0.125
    in
    Collage.group
        [ Collage.rect cellSize cellSize
            |> Collage.filled (Color.rgb 255 255 0)
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
