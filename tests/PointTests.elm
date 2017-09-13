module PointTests exposing (all)

import Expect
import Point
import Test exposing (..)


all : Test
all =
    describe "Point test suite"
        [ describe "add"
            [ test "adding a point to another works" <|
                \_ ->
                    Expect.equal ( 1, 0 ) (Point.add ( 0, 0 ) ( 1, 0 ))
            ]
        , describe "moveDown"
            [ test "moving a point down changes it's y value" <|
                \_ ->
                    Expect.equal ( 0, -1 ) (Point.moveDown ( 0, 0 ))
            ]
        ]
