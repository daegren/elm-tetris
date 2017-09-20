module Game exposing (..)

import Collage
import Color
import Element
import GameStyles
import Html exposing (Html, button, div, text)
import Html.CssHelpers
import Html.Events exposing (onClick)
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
    , lines : Int
    , interval : Float
    , tileBag : TileBag
    , state : State
    , seed : Random.Seed
    }


type State
    = NewGame
    | Playing
    | GameOver


type alias Level =
    Int


type alias Cell =
    { color : Color.Color
    , position : Point
    }


initialGame : Int -> Game
initialGame randomSeed =
    let
        seed =
            Random.initialSeed randomSeed

        bag =
            TileBag.init seed

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
    , score = 0
    , lines = 0
    , interval = 0
    , tileBag = bag1
    , state = NewGame
    , seed = seed
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


type Msg
    = StartGame


update : Msg -> Game -> Game
update msg game =
    case msg of
        StartGame ->
            let
                ( int, seed ) =
                    Random.step (Random.int 0 Random.maxInt) game.seed

                newGame =
                    initialGame int
            in
            { newGame | state = Playing }


level : Game -> Level
level { lines } =
    (floor <| toFloat lines / 10) + 1


speedForLevel : Level -> Float
speedForLevel level =
    -50 * toFloat level + 1000


tickGame : Float -> List Input.Key -> Game -> Game
tickGame delta keys game =
    case game.state of
        NewGame ->
            game

        Playing ->
            let
                currentInterval =
                    game.interval + delta
            in
            processInput keys game
                |> stepGame delta

        GameOver ->
            game


stepGame : Float -> Game -> Game
stepGame delta game =
    let
        currentInterval =
            game.interval + delta

        shouldStep =
            currentInterval > speedForLevel (level game)

        ( maybeCurrent, interval ) =
            if shouldStep then
                ( stepCurrent game game.current
                , currentInterval - speedForLevel (level game)
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
            if Tetromino.isAtSpawn game.current then
                { game | state = GameOver }
            else
                let
                    ( next, bag ) =
                        TileBag.pull game.tileBag

                    cells =
                        addCells game.current game.cells

                    lines =
                        fullRows -10 cells

                    score =
                        if lines == 1 then
                            100 * level game
                        else if lines == 2 then
                            300 * level game
                        else if lines == 3 then
                            500 * level game
                        else if lines == 4 then
                            800 * level game
                        else
                            0
                in
                { game
                    | interval = interval
                    , current = Tetromino.init game.nextPiece
                    , cells = checkCells -10 cells
                    , hasHeld = False
                    , score = game.score + score
                    , lines = game.lines + lines
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
    pieceCells ++ cells


getNumberOfCells : Float -> List Cell -> Int
getNumberOfCells row cells =
    List.filter (\c -> Tuple.second c.position == row) cells
        |> List.length


fullRows : Float -> List Cell -> Int
fullRows row cells =
    if getNumberOfCells row cells == 10 then
        1 + fullRows (row + 1) cells
    else if getNumberOfCells row cells /= 0 then
        0 + fullRows (row + 1) cells
    else
        0


checkCells : Float -> List Cell -> List Cell
checkCells row cells =
    let
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
    else if numberOfCells /= 0 then
        checkCells (row + 1) cells
    else
        cells


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
                    { game
                        | current = lowestPosition game game.current
                        , interval = game.interval + speedForLevel (level game)
                    }

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


view : Game -> Html Msg
view game =
    div [ id [ GameStyles.GameContainer ] ]
        [ overlayView game
        , div []
            [ nextPieceView game
            , heldPieceView game
            , scoreView game
            , lineView game
            , levelView game
            ]
        , div [ id [ GameStyles.PlayField ] ] [ playField game ]
        ]


overlayView : Game -> Html Msg
overlayView game =
    let
        overlay title actions =
            div [ id [ GameStyles.Overlay ] ]
                [ div [ class [ GameStyles.Title ] ] [ text title ]
                , div [ class [ GameStyles.Actions ] ]
                    (List.map (\( txt, msg ) -> button [ onClick msg ] [ text txt ]) actions)
                ]
    in
    case game.state of
        NewGame ->
            overlay "Welcome to Elm Tetris!" [ ( "New Game", StartGame ) ]

        GameOver ->
            overlay "Game Over!" [ ( "New Game", StartGame ) ]

        Playing ->
            div [] []


levelView : Game -> Html msg
levelView game =
    div []
        [ text "Level: "
        , text <| toString <| level game
        ]


lineView : Game -> Html msg
lineView { lines } =
    div []
        [ text "Lines: "
        , text <| toString lines
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
        , piecePreviewView heldPiece
        ]


nextPieceView : Game -> Html msg
nextPieceView { nextPiece } =
    let
        size =
            5 * cellSize
    in
    div []
        [ text "Next Piece"
        , piecePreviewView (Just nextPiece)
        ]


piecePreviewView : Maybe Shape -> Html msg
piecePreviewView maybeShape =
    let
        size =
            5 * cellSize
    in
    Element.toHtml <|
        Collage.collage size size <|
            [ backgroundView size size
            , case maybeShape of
                Just shape ->
                    let
                        offset =
                            previewOffset shape
                                |> (\( x, y ) -> ( x * cellSize, y * cellSize ))
                    in
                    Tetromino.view cellSize (Tetromino.init shape)
                        |> Collage.move offset

                Nothing ->
                    Element.empty |> Collage.toForm
            ]


previewOffset : Shape -> ( Float, Float )
previewOffset shape =
    case shape of
        I ->
            ( 0.5, 0 )

        O ->
            ( 0.5, 0.5 )

        _ ->
            ( 0, -0.5 )


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
