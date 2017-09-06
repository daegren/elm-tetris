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


type alias TileBag =
    List Shape


type alias Point =
    ( Int, Int )


type alias Cell =
    { color : Color.Color
    , position : Point
    }


initialGame : Game
initialGame =
    { current =
        { shape = J
        , position = spawnPosition
        }
    , nextPiece = O
    , cells = []
    , level = 1
    , interval = 0
    , tileBag = fullBag

    -- TODO: generate initialSeed from JS and pass through flags
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
            [ ( 0, 0 ), ( 1, 0 ), ( 0, -1 ), ( 1, -1 ) ]

        T ->
            [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( 0, -1 ) ]

        I ->
            [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( 0, -2 ) ]

        S ->
            [ ( 0, 0 ), ( 1, 0 ), ( 0, -1 ), ( -1, -1 ) ]

        Z ->
            [ ( 0, 0 ), ( -1, 0 ), ( 0, -1 ), ( 1, -1 ) ]

        J ->
            [ ( 0, 0 ), ( 0, -1 ), ( -1, -1 ), ( 0, 1 ) ]

        L ->
            [ ( 0, 0 ), ( 0, -1 ), ( 1, -1 ), ( 0, 1 ) ]


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
    cellsForShape tetromino.shape
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
                , current = Tetromino game.nextPiece spawnPosition
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
            x > -6
    in
    List.all (\cell -> isInsideGrid cell && wouldCollide game cell) cells


canMoveRight : Game -> Tetromino -> Bool
canMoveRight game tetromino =
    let
        cells =
            cellsForShape tetromino.shape
                |> List.map (add ( 1, 0 ))
                |> List.map (\c -> add c tetromino.position)

        isInsideGrid ( x, _ ) =
            x < 5
    in
    List.all (\cell -> isInsideGrid cell && wouldCollide game cell) cells


canMoveLower : Game -> Tetromino -> Bool
canMoveLower game tetromino =
    let
        cells =
            cellsForShape tetromino.shape
                |> List.map (add ( 0, -1 ))
                |> List.map (\c -> add c tetromino.position)

        isAboveGrid ( _, y ) =
            y > -11
    in
    List.all (\cell -> isAboveGrid cell && wouldCollide game cell) cells


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

                I ->
                    ( 0, cellSize / 2 )

                S ->
                    ( 0, cellSize / 2 )

                Z ->
                    ( 0, cellSize / 2 )

                J ->
                    ( cellSize / 2, 0 )

                L ->
                    ( -cellSize / 2, 0 )
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
