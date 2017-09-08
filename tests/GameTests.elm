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
                    \_ -> Expect.equalLists (Game.cellsForShape Game.I) (Game.rotatePoints Game.Up Game.I)
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
            ]
        ]
