module Game exposing (..)

import Collage
import Color
import Element
import GameStyles
import Html exposing (Html, div, text)
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
    , heldPiece : Maybe Shape
    , hasHeld : Bool
    , cells : List Cell
    , score : Int
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
    , heldPiece = Nothing
    , hasHeld = False
    , cells = []
    , level = 1
    , score = 0
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
    in
    processInput keys game
        |> stepGame delta


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
                , hasHeld = False
                , nextPiece = next
                , tileBag = bag
            }


addCells : Tetromino -> List Cell -> List Cell
addCells tetromino cells =
    let
        pieceCells =
            toCells tetromino

        getNumberOfCells row cells =
            List.filter (\c -> Tuple.second c.position == row) cells
                |> List.length
    in
    checkCells -10 (pieceCells ++ cells)


checkCells : Float -> List Cell -> List Cell
checkCells row cells =
    let
        getNumberOfCells row cells =
            List.filter (\c -> Tuple.second c.position == row) cells
                |> List.length

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

        numberOfCells =
            getNumberOfCells row cells
    in
    if numberOfCells == 10 then
        checkCells row (removeRow row cells)
    else if numberOfCells == 0 then
        cells
    else
        checkCells (row + 1) cells


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


processInput : List Input.Key -> Game -> Game
processInput keys game =
    List.foldl
        (\k game ->
            case k of
                Input.Left ->
                    if canMoveLeft game game.current then
                        { game | current = Tetromino.moveLeft game.current }
                    else
                        game

                Input.Right ->
                    if canMoveRight game game.current then
                        { game | current = Tetromino.moveRight game.current }
                    else
                        game

                Input.RotateClockwise ->
                    if canRotate game Tetromino.Clockwise game.current then
                        { game | current = Tetromino.rotate Tetromino.Clockwise game.current }
                    else
                        game

                Input.RotateCounterClockwise ->
                    if canRotate game Tetromino.CounterClockwise game.current then
                        { game | current = Tetromino.rotate Tetromino.CounterClockwise game.current }
                    else
                        game

                Input.HardDrop ->
                    { game | current = lowestPosition game game.current }

                Input.Hold ->
                    if not game.hasHeld then
                        swapHeld game
                    else
                        game
        )
        game
        keys


swapHeld : Game -> Game
swapHeld game =
    let
        newGame =
            case game.heldPiece of
                Just piece ->
                    { game | current = Tetromino.init piece, heldPiece = Just (Tetromino.shape game.current) }

                Nothing ->
                    let
                        ( nextPiece, bag ) =
                            TileBag.pull game.tileBag
                    in
                    { game
                        | current = Tetromino.init game.nextPiece
                        , nextPiece = nextPiece
                        , heldPiece = Just (Tetromino.shape game.current)
                        , tileBag = bag
                    }
    in
    { newGame | hasHeld = True }


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
        [ div []
            [ nextPieceView game
            , heldPieceView game
            , scoreView game
            ]
        , div [ id [ GameStyles.PlayField ] ] [ playField game ]
        ]


scoreView : Game -> Html msg
scoreView game =
    div []
        [ text "Score: "
        , text <| toString game.score
        ]


heldPieceView : Game -> Html msg
heldPieceView { heldPiece } =
    let
        size =
            5 * cellSize
    in
    div []
        [ text "Hold Piece"
        , Element.toHtml <|
            Collage.collage size size <|
                [ backgroundView size size
                , case heldPiece of
                    Just piece ->
                        Tetromino.view cellSize (Tetromino.init piece)

                    Nothing ->
                        Element.empty |> Collage.toForm
                ]
        ]


nextPieceView : Game -> Html msg
nextPieceView { nextPiece } =
    let
        size =
            5 * cellSize
    in
    div []
        [ text "Next Piece"
        , Element.toHtml <|
            Collage.collage size
                size
                [ backgroundView size size
                , Tetromino.view cellSize (Tetromino.init nextPiece)
                ]
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
        Collage.collage width height <|
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
