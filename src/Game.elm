module Game exposing (..)

import Collage
import Color
import Element
import GameStyles
import Html exposing (Html, div)
import Html.CssHelpers
import Input


-- MODEL


type alias Game =
    { current : Tetromino
    , nextPiece : Shape
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
    , nextPiece = Circle
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

        newGame =
            tickCurrent delta game
    in
    { newGame | current = processInput newGame.current keys }


tickCurrent : Float -> Game -> Game
tickCurrent delta game =
    let
        currentInterval =
            game.interval + delta

        ( current, interval ) =
            if currentInterval > speedForLevel game.level then
                ( moveDown game.current, currentInterval - speedForLevel game.level )
            else
                ( game.current, currentInterval )
    in
    { game | interval = interval, current = current }


moveDown : Tetromino -> Tetromino
moveDown tetromino =
    let
        position =
            if canMoveLower tetromino then
                tetromino.position
                    |> Tuple.mapSecond (\y -> y - 1)
            else
                tetromino.position
    in
    { tetromino | position = position }


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
        |> List.any (\( x, _ ) -> x > -4)


canMoveRight : Tetromino -> Bool
canMoveRight tetromino =
    let
        cells =
            cellsForShape tetromino.shape
    in
    List.map (\c -> add c tetromino.position) cells
        |> List.map (add ( 1, 0 ))
        |> List.any (\( x, _ ) -> x < 3)


canMoveLower : Tetromino -> Bool
canMoveLower tetromino =
    let
        cells =
            cellsForShape tetromino.shape
    in
    List.map (\c -> add c tetromino.position) cells
        |> List.any (\( x, y ) -> y > -9)



-- CSS HELEPERS


{ id, class, classList } =
    Html.CssHelpers.withNamespace GameStyles.ns



-- VIEW


view : Game -> Html msg
view game =
    div [ id [ GameStyles.GameContainer ] ]
        [ nextPieceView game
        , div [ id [ GameStyles.PlayField ] ] [ playField game ]
        ]


nextPieceView : Game -> Html msg
nextPieceView { nextPiece } =
    let
        size =
            5 * cellSize

        offset =
            case nextPiece of
                Circle ->
                    ( -cellSize / 2, cellSize / 2 )

                T ->
                    ( 0, 0 )
    in
    Element.toHtml <|
        Collage.collage size
            size
            [ backgroundView size size
            , tetrominoCell nextPiece
                |> Collage.move offset
            ]


playField : Game -> Html msg
playField game =
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
            , currentView game.current
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


currentView : Tetromino -> Collage.Form
currentView current =
    let
        ( posX, posY ) =
            current.position

        x =
            toFloat (posX * cellSize) + (cellSize / 2)

        y =
            toFloat (posY * cellSize) + (cellSize / 2)
    in
    tetrominoCell current.shape
        |> Collage.move ( x, y )


tetrominoCell : Shape -> Collage.Form
tetrominoCell shape =
    cellsForShape shape
        |> List.map
            (\( x, y ) ->
                cell (colorForShape shape)
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
