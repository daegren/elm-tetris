module Point exposing (Point, add, moveDown)

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
