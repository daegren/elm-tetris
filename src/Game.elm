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
    , cells : List Cell
    , level : Level
    , interval : Float
    }


type alias Tetromino =
    { shape : Shape
    , position : Point
    }


type alias Level =
    Int


type Shape
    = O
    | T


type alias Point =
    ( Int, Int )


type alias Cell =
    { color : Color.Color
    , position : Point
    }


initialGame : Game
initialGame =
    { current =
        { shape = T
        , position = spawnPosition
        }
    , nextPiece = O
    , cells = []
    , level = 1
    , interval = 0
    }


spawnPosition : Point
spawnPosition =
    ( -1, 9 )


cellsForShape : Shape -> List Point
cellsForShape shape =
    case shape of
        O ->
            [ ( 0, 0 ), ( 1, 0 ), ( 0, -1 ), ( 1, -1 ) ]

        T ->
            [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( 0, -1 ) ]


colorForShape : Shape -> Color.Color
colorForShape shape =
    case shape of
        O ->
            Color.rgb 255 255 0

        T ->
            Color.rgb 255 0 255


toCells : Tetromino -> List Cell
toCells tetromino =
    let
        toCell color position =
            Cell color position
    in
    cellsForShape tetromino.shape
        |> List.map (add tetromino.position)
        |> List.map (toCell (colorForShape tetromino.shape))


add : Point -> Point -> Point
add ( ax, ay ) ( bx, by ) =
    ( ax + bx, ay + by )



-- UPDATE


speedForLevel : Level -> Float
speedForLevel level =
    250


tickGame : Float -> List Input.Key -> Game -> Game
tickGame delta keys game =
    let
        currentInterval =
            game.interval + delta

        newGame =
            stepGame delta game
    in
    { newGame | current = processInput newGame newGame.current keys }


stepGame : Float -> Game -> Game
stepGame delta game =
    let
        currentInterval =
            game.interval + delta

        shouldStep =
            currentInterval > speedForLevel game.level

        ( maybeCurrent, interval ) =
            if shouldStep then
                ( stepCurrent game game.current
                , currentInterval - speedForLevel game.level
                )
            else
                ( Just game.current, currentInterval )

        ( cells, current ) =
            case maybeCurrent of
                Just c ->
                    ( game.cells, c )

                Nothing ->
                    ( toCells game.current ++ game.cells, Tetromino T spawnPosition )
    in
    { game
        | interval = interval
        , current = current
        , cells = cells
    }


stepCurrent : Game -> Tetromino -> Maybe Tetromino
stepCurrent game tetromino =
    if canMoveLower game tetromino then
        Just (moveDown tetromino)
    else
        -- Cannot move tetromino down, add it to the cell list and generate a new tetromino
        Nothing


moveDown : Tetromino -> Tetromino
moveDown tetromino =
    { tetromino | position = Tuple.mapSecond (\y -> y - 1) tetromino.position }


processInput : Game -> Tetromino -> List Input.Key -> Tetromino
processInput game tetromino =
    List.foldl
        (\k c ->
            case k of
                Input.Left ->
                    if canMoveLeft game c then
                        { c | position = add ( -1, 0 ) c.position }
                    else
                        c

                Input.Right ->
                    if canMoveRight game c then
                        { c | position = add ( 1, 0 ) c.position }
                    else
                        c
        )
        tetromino


canMoveLeft : Game -> Tetromino -> Bool
canMoveLeft game tetromino =
    let
        cells =
            cellsForShape tetromino.shape
                |> List.map (add ( -1, 0 ))
                |> List.map (\c -> add c tetromino.position)

        isInsideGrid ( x, _ ) =
            x > -4
    in
    List.any isInsideGrid cells && List.all (wouldCollide game) cells


canMoveRight : Game -> Tetromino -> Bool
canMoveRight game tetromino =
    let
        cells =
            cellsForShape tetromino.shape
                |> List.map (add ( 1, 0 ))
                |> List.map (\c -> add c tetromino.position)

        isInsideGrid ( x, _ ) =
            x < 3
    in
    List.any isInsideGrid cells && List.all (wouldCollide game) cells


canMoveLower : Game -> Tetromino -> Bool
canMoveLower game tetromino =
    let
        cells =
            cellsForShape tetromino.shape
                |> List.map (add ( 0, -1 ))
                |> List.map (\c -> add c tetromino.position)

        isAboveGrid ( _, y ) =
            y > -10
    in
    List.any isAboveGrid cells && List.all (wouldCollide game) cells


wouldCollide : Game -> Point -> Bool
wouldCollide game p =
    List.map .position game.cells
        |> List.all ((/=) p)



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
                O ->
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
            , cellsView game.cells
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


position : { a | position : Point } -> ( Float, Float )
position tetromino =
    let
        ( posX, posY ) =
            tetromino.position

        x =
            toFloat (posX * cellSize) + (cellSize / 2)

        y =
            toFloat (posY * cellSize) + (cellSize / 2)
    in
    ( toFloat (posX * cellSize) + (cellSize / 2)
    , toFloat (posY * cellSize) + (cellSize / 2)
    )


cellsView : List Cell -> Collage.Form
cellsView cells =
    List.map cellView cells
        |> Collage.group


cellView : Cell -> Collage.Form
cellView c =
    cell c.color
        |> Collage.move (position c)


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
