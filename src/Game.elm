module Game exposing (..)

import Collage
import Color
import Element
import Html exposing (Html)
import Input


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
    | T


initialGame : Game
initialGame =
    { current =
        { shape = T
        , position = ( -1, 9 )
        }
    , level = 1
    , interval = 0
    }


cellsForShape : Shape -> List ( Int, Int )
cellsForShape shape =
    case shape of
        Circle ->
            [ ( 0, 0 ), ( 1, 0 ), ( 0, -1 ), ( 1, -1 ) ]

        T ->
            [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( 0, -1 ) ]


colorForShape : Shape -> Color.Color
colorForShape shape =
    case shape of
        Circle ->
            Color.rgb 255 255 0

        T ->
            Color.rgb 255 0 255


add : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
add ( ax, ay ) ( bx, by ) =
    ( ax + bx, ay + by )



-- UPDATE


speedForLevel : Level -> Float
speedForLevel level =
    1000


tickGame : Float -> List Input.Key -> Game -> Game
tickGame delta keys game =
    let
        currentInterval =
            game.interval + delta

        ( current, interval ) =
            if currentInterval > speedForLevel game.level then
                ( tickCurrent game.current, currentInterval - speedForLevel game.level )
            else
                ( game.current, currentInterval )
    in
    { game | interval = interval, current = processInput current keys }


processInput : Tetromino -> List Input.Key -> Tetromino
processInput tetromino =
    List.foldl
        (\k c ->
            case k of
                Input.Left ->
                    if canMoveLeft c then
                        { c | position = add ( -1, 0 ) c.position }
                    else
                        c

                Input.Right ->
                    if canMoveRight c then
                        { c | position = add ( 1, 0 ) c.position }
                    else
                        c
        )
        tetromino


canMoveLeft : Tetromino -> Bool
canMoveLeft tetromino =
    let
        cells =
            cellsForShape tetromino.shape
    in
    List.map (\c -> add c tetromino.position) cells
        |> List.map (add ( -1, 0 ))
        |> Debug.log "Can Move Left cells"
        |> List.any (\( x, _ ) -> x > -4)


canMoveRight : Tetromino -> Bool
canMoveRight tetromino =
    let
        cells =
            cellsForShape tetromino.shape
    in
    List.map (\c -> add c tetromino.position) cells
        |> List.map (add ( 1, 0 ))
        |> Debug.log "Can Move Right cells"
        |> List.any (\( x, _ ) -> x < 3)


tickCurrent : Tetromino -> Tetromino
tickCurrent tetromino =
    let
        position =
            if canMoveLower tetromino then
                tetromino.position
                    |> Tuple.mapSecond (\y -> y - 1)
            else
                tetromino.position
    in
    { tetromino | position = position }


canMoveLower : Tetromino -> Bool
canMoveLower tetromino =
    let
        cells =
            cellsForShape tetromino.shape
    in
    List.map (\c -> add c tetromino.position) cells
        |> List.any (\( x, y ) -> y > -9)



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
    cellsForShape tetromino.shape
        |> List.map
            (\( x, y ) ->
                cell (colorForShape tetromino.shape)
                    |> Collage.move ( cellSize * toFloat x, cellSize * toFloat y )
            )
        |> Collage.group


cell : Color.Color -> Collage.Form
cell cellColor =
    let
        half =
            cellSize / 2

        embossWidth =
            cellSize * 0.125
    in
    Collage.group
        [ Collage.rect cellSize cellSize
            |> Collage.filled cellColor
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
