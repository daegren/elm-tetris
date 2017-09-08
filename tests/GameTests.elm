module GameTests exposing (all)

import Expect
import Game
import Test exposing (..)


all : Test
all =
    describe "Game Test Suite"
        [ describe "rotating piece"
            [ describe "I"
                [ test "0 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Up Game.I
                            |> Expect.equalLists (Game.cellsForShape Game.I)
                , test "90 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Right Game.I
                            |> Expect.equalLists [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( 0, -2 ) ]
                , test "180 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Down Game.I
                            |> Expect.equalLists [ ( 1, -1 ), ( 0, -1 ), ( -1, -1 ), ( -2, -1 ) ]
                , test "270 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Left Game.I
                            |> Expect.equalLists [ ( -1, -2 ), ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
                ]
            , describe "J"
                [ test "0 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Up Game.J
                            |> Expect.equalLists (Game.cellsForShape Game.J)
                , test "90 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Right Game.J
                            |> Expect.equalLists [ ( 1, 1 ), ( 0, 1 ), ( 0, 0 ), ( 0, -1 ) ]
                , test "180 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Down Game.J
                            |> Expect.equalLists [ ( 1, -1 ), ( 1, 0 ), ( 0, 0 ), ( -1, 0 ) ]
                , test "270 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Left Game.J
                            |> Expect.equalLists [ ( -1, -1 ), ( 0, -1 ), ( 0, 0 ), ( 0, 1 ) ]
                ]
            , describe "L"
                [ test "0 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Up Game.L
                            |> Expect.equalLists (Game.cellsForShape Game.L)
                , test "90 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Right Game.L
                            |> Expect.equalLists [ ( 0, 1 ), ( 0, 0 ), ( 0, -1 ), ( 1, -1 ) ]
                , test "180 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Down Game.L
                            |> Expect.equalLists [ ( 1, 0 ), ( 0, 0 ), ( -1, 0 ), ( -1, -1 ) ]
                , test "270 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Left Game.L
                            |> Expect.equalLists [ ( 0, -1 ), ( 0, 0 ), ( 0, 1 ), ( -1, 1 ) ]
                ]
            , describe "O"
                [ test "0 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Up Game.O
                            |> Expect.equalLists (Game.cellsForShape Game.O)
                , test "90 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Right Game.O
                            |> Expect.equalLists [ ( 0, 0 ), ( 0, -1 ), ( -1, -1 ), ( -1, 0 ) ]
                , test "180 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Down Game.O
                            |> Expect.equalLists [ ( 0, -1 ), ( -1, -1 ), ( -1, 0 ), ( 0, 0 ) ]
                , test "270 degree rotation" <|
                    \_ ->
                        Game.rotatePoints Game.Left Game.O
                            |> Expect.equalLists [ ( -1, -1 ), ( -1, 0 ), ( 0, 0 ), ( 0, -1 ) ]
                ]
            ]
        ]
