module Game exposing (..)

import Collage
import Color
import Element
import GameStyles
import Html exposing (Html, div)
import Html.CssHelpers
import Input
import Random


-- MODEL


type alias Game =
    { current : Tetromino
    , nextPiece : Shape
    , cells : List Cell
    , level : Level
    , interval : Float
    , tileBag : TileBag
    , seed : Random.Seed
    }


type alias Tetromino =
    { shape : Shape
    , position : Point
    , direction : Direction
    }


type alias Level =
    Int


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


type alias TileBag =
    List Shape


type alias Point =
    ( Float, Float )


type alias Cell =
    { color : Color.Color
    , position : Point
    }


initialGame : Game
initialGame =
    let
        -- TODO: generate initialSeed from JS and pass through flags
        seed0 =
            Random.initialSeed 1234

        ( initialPiece, seed1 ) =
            Random.step (nextPiece fullBag) seed0

        bag0 =
            List.filter ((/=) initialPiece) fullBag

        ( next, seed2 ) =
            Random.step (nextPiece bag0) seed1

        bag1 =
            List.filter ((/=) next) bag0
    in
    { current =
        { shape = initialPiece
        , position = spawnPosition
        , direction = Up
        }
    , nextPiece = next
    , cells = []
    , level = 1
    , interval = 0
    , tileBag = bag1
    , seed = Random.initialSeed 1234
    }


fullBag : TileBag
fullBag =
    [ O, T, I, S, Z, J, L ]


spawnPosition : Point
spawnPosition =
    ( -1, 9 )


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


points : Tetromino -> List Point
points tetromino =
    rotatePoints tetromino.direction tetromino.shape


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


toCells : Tetromino -> List Cell
toCells tetromino =
    let
        toCell color position =
            Cell color position
    in
    points tetromino
        |> List.map (add tetromino.position)
        |> List.map (toCell (colorForShape tetromino.shape))


add : Point -> Point -> Point
add ( ax, ay ) ( bx, by ) =
    ( ax + bx, ay + by )



-- GENERATORS


nextPiece : TileBag -> Random.Generator Shape
nextPiece tileBag =
    Random.map (Maybe.withDefault O) (oneOf tileBag)


oneOf : List a -> Random.Generator (Maybe a)
oneOf list =
    Random.map
        (\i ->
            List.indexedMap (,) list
                |> List.filter (\( idx, _ ) -> idx == i)
                |> List.map Tuple.second
                |> List.head
        )
        (Random.int 0 (List.length list - 1))



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
                bag =
                    if List.isEmpty game.tileBag then
                        fullBag
                    else
                        game.tileBag

                ( p, seed ) =
                    Random.step (nextPiece bag) game.seed
            in
            { game
                | interval = interval
                , current = Tetromino game.nextPiece spawnPosition Up
                , cells = toCells game.current ++ game.cells
                , nextPiece = p
                , seed = seed
                , tileBag = List.filter ((/=) p) bag
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

                Input.RotateClockwise ->
                    if canRotate game Clockwise c then
                        rotate Clockwise c
                    else
                        c

                Input.RotateCounterClockwise ->
                    if canRotate game CounterClockwise c then
                        rotate CounterClockwise c
                    else
                        c
        )
        tetromino


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
        List.map (add offset) points
            |> List.map toPolar
            |> List.map (Tuple.mapSecond (\a -> a - turns (0.25 * numOfTurns)))
            |> List.map fromPolar
            |> List.map (add negativeOffset)
            |> List.map (\( x, y ) -> ( fixRoundingErrors x, fixRoundingErrors y ))


mapTuple : (a -> b -> ( a1, b1 )) -> ( a, b ) -> ( a1, b1 )
mapTuple mapper ( a, b ) =
    mapper a b


fixRoundingErrors : Float -> Float
fixRoundingErrors a =
    round a |> toFloat


rotate : Rotation -> Tetromino -> Tetromino
rotate rotation tetromino =
    let
        direction =
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
    in
    { tetromino | direction = direction }


canRotate : Game -> Rotation -> Tetromino -> Bool
canRotate game rotation tetromino =
    let
        cells =
            rotate rotation tetromino
                |> points
                |> List.map (\c -> add c tetromino.position)
    in
    List.all (\cell -> isInsideGrid cell && wouldCollide game cell) cells


canMoveLeft : Game -> Tetromino -> Bool
canMoveLeft game tetromino =
    let
        cells =
            points tetromino
                |> List.map (add ( -1, 0 ))
                |> List.map (\c -> add c tetromino.position)
    in
    List.all (\cell -> isInsideGrid cell && wouldCollide game cell) cells


canMoveRight : Game -> Tetromino -> Bool
canMoveRight game tetromino =
    let
        cells =
            points tetromino
                |> List.map (add ( 1, 0 ))
                |> List.map (\c -> add c tetromino.position)
    in
    List.all (\cell -> isInsideGrid cell && wouldCollide game cell) cells


canMoveLower : Game -> Tetromino -> Bool
canMoveLower game tetromino =
    let
        cells =
            points tetromino
                |> List.map (add ( 0, -1 ))
                |> List.map (\c -> add c tetromino.position)
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
            , tetrominoCell nextPiece Up
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


position : { a | position : Point } -> ( Float, Float )
position tetromino =
    let
        ( posX, posY ) =
            tetromino.position

        x =
            (posX * cellSize) + (cellSize / 2)

        y =
            (posY * cellSize) + (cellSize / 2)
    in
    ( x, y )


ghostView : Game -> Tetromino -> Collage.Form
ghostView game tetromino =
    let
        calcPosition t =
            if canMoveLower game t then
                calcPosition (moveDown t)
            else
                t

        finalT =
            calcPosition tetromino
    in
    tetrominoCell finalT.shape finalT.direction
        |> Collage.alpha 0.375
        |> Collage.move (position finalT)


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
            (posX * cellSize) + (cellSize / 2)

        y =
            (posY * cellSize) + (cellSize / 2)
    in
    tetrominoCell current.shape current.direction
        |> Collage.move ( x, y )


tetrominoCell : Shape -> Direction -> Collage.Form
tetrominoCell shape direction =
    rotatePoints direction shape
        |> List.map
            (\( x, y ) ->
                cell (colorForShape shape)
                    |> Collage.move ( cellSize * x, cellSize * y )
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
