module Game exposing (..)

import Collage
import Color
import Element
import GameStyles
import Html exposing (Html, div)
import Html.CssHelpers
import Input
import Point exposing (Point)
import Random
import Tetromino exposing (Direction, Shape(..), Tetromino)
import TileBag exposing (TileBag)


-- MODEL


type alias Game =
    { current : Tetromino
    , nextPiece : Shape
    , cells : List Cell
    , level : Level
    , interval : Float
    , tileBag : TileBag
    }


type alias Level =
    Int


type alias Cell =
    { color : Color.Color
    , position : Point
    }


initialGame : Game
initialGame =
    let
        -- TODO: generate initialSeed from JS and pass through flags
        bag =
            TileBag.init (Random.initialSeed 1234)

        ( initialShape, bag0 ) =
            TileBag.pull bag

        ( nextShape, bag1 ) =
            TileBag.pull bag0
    in
    { current =
        Tetromino.init initialShape
    , nextPiece = nextShape
    , cells = []
    , level = 1
    , interval = 0
    , tileBag = bag1
    }


toCells : Tetromino -> List Cell
toCells tetromino =
    let
        toCell color position =
            Cell color position
    in
    Tetromino.points tetromino
        |> List.map (Point.add (Tetromino.position tetromino))
        |> List.map (toCell (Tetromino.color tetromino))



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
    in
    case maybeCurrent of
        Just c ->
            { game
                | interval = interval
                , current = c
            }

        Nothing ->
            let
                ( next, bag ) =
                    TileBag.pull game.tileBag
            in
            { game
                | interval = interval
                , current = Tetromino.init game.nextPiece
                , cells = addCells game.current game.cells
                , nextPiece = next
                , tileBag = bag
            }


addCells : Tetromino -> List Cell -> List Cell
addCells tetromino cells =
    let
        pieceCells =
            toCells tetromino

        checkRow row cells =
            List.filter (\c -> Tuple.second c.position == row) cells
                |> List.length
                |> (==) 10

        removeRow row cells =
            List.map
                (\c ->
                    let
                        ( _, y ) =
                            c.position
                    in
                    if y == row then
                        Nothing
                    else if y > row then
                        Just (moveDown c)
                    else
                        Just c
                )
                cells
                |> List.filterMap identity
    in
    List.foldl
        (\c cs ->
            let
                ( _, y ) =
                    c.position
            in
            if checkRow y cs then
                removeRow y cs
            else
                cs
        )
        (pieceCells ++ cells)
        pieceCells


stepCurrent : Game -> Tetromino -> Maybe Tetromino
stepCurrent game tetromino =
    if canMoveLower game tetromino then
        Just (Tetromino.moveDown tetromino)
    else
        -- Cannot move tetromino down, add it to the cell list and generate a new tetromino
        Nothing


moveDown : { a | position : Point } -> { a | position : Point }
moveDown a =
    { a | position = Point.moveDown a.position }


processInput : Game -> Tetromino -> List Input.Key -> Tetromino
processInput game tetromino =
    List.foldl
        (\k c ->
            case k of
                Input.Left ->
                    if canMoveLeft game c then
                        Tetromino.moveLeft c
                    else
                        c

                Input.Right ->
                    if canMoveRight game c then
                        Tetromino.moveRight c
                    else
                        c

                Input.RotateClockwise ->
                    if canRotate game Tetromino.Clockwise c then
                        Tetromino.rotate Tetromino.Clockwise c
                    else
                        c

                Input.RotateCounterClockwise ->
                    if canRotate game Tetromino.CounterClockwise c then
                        Tetromino.rotate Tetromino.CounterClockwise c
                    else
                        c

                Input.HardDrop ->
                    lowestPosition game c
        )
        tetromino


canRotate : Game -> Tetromino.Rotation -> Tetromino -> Bool
canRotate game rotation tetromino =
    let
        cells =
            Tetromino.rotate rotation tetromino
                |> Tetromino.points
                |> List.map (\c -> Point.add c (Tetromino.position tetromino))
    in
    List.all (\cell -> isInsideGrid cell && wouldCollide game cell) cells


canMoveLeft : Game -> Tetromino -> Bool
canMoveLeft game tetromino =
    let
        cells =
            Tetromino.points tetromino
                |> List.map (Point.add ( -1, 0 ))
                |> List.map (\c -> Point.add c (Tetromino.position tetromino))
    in
    List.all (\cell -> isInsideGrid cell && wouldCollide game cell) cells


canMoveRight : Game -> Tetromino -> Bool
canMoveRight game tetromino =
    let
        cells =
            Tetromino.points tetromino
                |> List.map (Point.add ( 1, 0 ))
                |> List.map (\c -> Point.add c (Tetromino.position tetromino))
    in
    List.all (\cell -> isInsideGrid cell && wouldCollide game cell) cells


canMoveLower : Game -> Tetromino -> Bool
canMoveLower game tetromino =
    let
        cells =
            Tetromino.points tetromino
                |> List.map (Point.add ( 0, -1 ))
                |> List.map (\c -> Point.add c (Tetromino.position tetromino))
    in
    List.all (\cell -> isInsideGrid cell && wouldCollide game cell) cells


wouldCollide : Game -> Point -> Bool
wouldCollide game p =
    List.map .position game.cells
        |> List.all ((/=) p)


isInsideGrid : Point -> Bool
isInsideGrid ( x, y ) =
    (x > -6)
        && (x < 5)
        && (y > -11)


lowestPosition : Game -> Tetromino -> Tetromino
lowestPosition game tetromino =
    if canMoveLower game tetromino then
        lowestPosition game (Tetromino.moveDown tetromino)
    else
        tetromino



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
    in
    Element.toHtml <|
        Collage.collage size
            size
            [ backgroundView size size
            , Tetromino.view cellSize (Tetromino.init nextPiece)
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
            , ghostView game game.current
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


position : Point -> ( Float, Float )
position ( posX, posY ) =
    let
        x =
            (posX * cellSize) + (cellSize / 2)

        y =
            (posY * cellSize) + (cellSize / 2)
    in
    ( x, y )


ghostView : Game -> Tetromino -> Collage.Form
ghostView game tetromino =
    Tetromino.view cellSize tetromino
        |> Collage.alpha 0.375
        |> (lowestPosition game tetromino
                |> Tetromino.position
                |> position
                |> Collage.move
           )


cellsView : List Cell -> Collage.Form
cellsView cells =
    List.map cellView cells
        |> Collage.group


cellView : Cell -> Collage.Form
cellView c =
    Tetromino.cell cellSize c.color
        |> Collage.move (position c.position)


currentView : Tetromino -> Collage.Form
currentView current =
    let
        ( posX, posY ) =
            Tetromino.position current

        x =
            (posX * cellSize) + (cellSize / 2)

        y =
            (posY * cellSize) + (cellSize / 2)
    in
    Tetromino.view cellSize current
        |> Collage.move ( x, y )
