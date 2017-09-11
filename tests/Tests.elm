module Tests exposing (..)

import GameTests
import Test exposing (..)


all : Test
all =
    describe "Tetris Test Suite"
        [ GameTests.all ]
