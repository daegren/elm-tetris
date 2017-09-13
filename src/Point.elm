module Point exposing (Point, add, moveDown, moveLeft, moveRight)

{-| Helpers for representing a point in a 2d grid system
-}


{-| Point represents a point in a grid
-}
type alias Point =
    ( Float, Float )


{-| Add one point to another

    (1, 0) == add (0, 0) (1, 0)
    (10, 5) == add (5, 2) (5, 3)
    (0, 5) == add (5, 5) (-5, 0)

-}
add : Point -> Point -> Point
add ( ax, ay ) ( bx, by ) =
    ( ax + bx, ay + by )


{-| Move a point down on the y-axis

    (0, -1) == moveDown (0, 0)

-}
moveDown : Point -> Point
moveDown =
    Tuple.mapSecond (\y -> y - 1)


{-| Move a point to the left on the x-axis

    (-1, 0) == moveLeft (0, 0)

-}
moveLeft : Point -> Point
moveLeft =
    Tuple.mapFirst (\x -> x - 1)


{-| Move a point to the right on the x-axis

    (1, 0) == moveRight (0, 0)

-}
moveRight : Point -> Point
moveRight =
    Tuple.mapFirst (\x -> x + 1)
