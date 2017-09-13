module TetrominoTests exposing (all)

import Expect
import Test exposing (..)
import Tetromino


all : Test
all =
    describe "Tetromino Test Suite"
        [ describe "rotating piece"
            [ describe "I"
                [ test "0 degree rotation" <|
                    \_ ->
                        Tetromino.points (tetromino Tetromino.I)
                            |> Expect.equalLists [ ( -2, 0 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]
                , test "90 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.I
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( 0, -2 ) ]
                , test "180 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.I
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 1, -1 ), ( 0, -1 ), ( -1, -1 ), ( -2, -1 ) ]
                , test "270 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.I
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( -1, -2 ), ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
                ]
            , describe "J"
                [ test "0 degree rotation" <|
                    \_ ->
                        Tetromino.points (tetromino Tetromino.J)
                            |> Expect.equalLists [ ( -1, 1 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]
                , test "90 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.J
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 1, 1 ), ( 0, 1 ), ( 0, 0 ), ( 0, -1 ) ]
                , test "180 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.J
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 1, -1 ), ( 1, 0 ), ( 0, 0 ), ( -1, 0 ) ]
                , test "270 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.J
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( -1, -1 ), ( 0, -1 ), ( 0, 0 ), ( 0, 1 ) ]
                ]
            , describe "L"
                [ test "0 degree rotation" <|
                    \_ ->
                        Tetromino.points (tetromino Tetromino.L)
                            |> Expect.equalLists [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 1, 1 ) ]
                , test "90 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.L
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( 1, -1 ) ]
                , test "180 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.L
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 1, 0 ), ( 0, 0 ), ( -1, 0 ), ( -1, -1 ) ]
                , test "270 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.L
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 0, -1 ), ( 0, 0 ), ( 0, 1 ), ( -1, 1 ) ]
                ]
            , describe "O"
                [ test "0 degree rotation" <|
                    \_ ->
                        Tetromino.points (tetromino Tetromino.O)
                            |> Expect.equalLists [ ( -1, 0 ), ( 0, 0 ), ( 0, -1 ), ( -1, -1 ) ]
                , test "90 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.O
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 0, 0 ), ( 0, -1 ), ( -1, -1 ), ( -1, 0 ) ]
                , test "180 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.O
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 0, -1 ), ( -1, -1 ), ( -1, 0 ), ( 0, 0 ) ]
                , test "270 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.O
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( -1, -1 ), ( -1, 0 ), ( 0, 0 ), ( 0, -1 ) ]
                ]
            , describe "S"
                [ test "0 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.S
                            |> Tetromino.points
                            |> Expect.equalLists [ ( -1, 0 ), ( 0, 0 ), ( 0, 1 ), ( 1, 1 ) ]
                , test "90 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.S
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 0, 1 ), ( 0, 0 ), ( 1, 0 ), ( 1, -1 ) ]
                , test "180 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.S
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 1, 0 ), ( 0, 0 ), ( 0, -1 ), ( -1, -1 ) ]
                , test "270 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.S
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 0, -1 ), ( 0, 0 ), ( -1, 0 ), ( -1, 1 ) ]
                ]
            , describe "T"
                [ test "0 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.T
                            |> Tetromino.points
                            |> Expect.equalLists [ ( -1, 0 ), ( 0, 0 ), ( 0, 1 ), ( 1, 0 ) ]
                , test "90 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.T
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 0, 1 ), ( 0, 0 ), ( 1, 0 ), ( 0, -1 ) ]
                , test "180 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.T
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 1, 0 ), ( 0, 0 ), ( 0, -1 ), ( -1, 0 ) ]
                , test "270 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.T
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 0, -1 ), ( 0, 0 ), ( -1, 0 ), ( 0, 1 ) ]
                ]
            , describe "Z"
                [ test "0 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.Z
                            |> Tetromino.points
                            |> Expect.equalLists [ ( -1, 1 ), ( 0, 1 ), ( 0, 0 ), ( 1, 0 ) ]
                , test "90 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.Z
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 1, 1 ), ( 1, 0 ), ( 0, 0 ), ( 0, -1 ) ]
                , test "180 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.Z
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( 1, -1 ), ( 0, -1 ), ( 0, 0 ), ( -1, 0 ) ]
                , test "270 degree rotation" <|
                    \_ ->
                        tetromino Tetromino.Z
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.rotate Tetromino.Clockwise
                            |> Tetromino.points
                            |> Expect.equalLists [ ( -1, -1 ), ( -1, 0 ), ( 0, 0 ), ( 0, 1 ) ]
                ]
            ]
        ]


tetromino : Tetromino.Shape -> Tetromino.Tetromino
tetromino shape =
    Tetromino.init shape
