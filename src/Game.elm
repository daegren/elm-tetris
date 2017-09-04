module Game exposing (..)

import Collage
import Color
import Element
import Html exposing (Html)


-- MODEL


type alias Game =
    { current : Tetromino
    , level : Int
    }


type alias Tetromino =
    { shape : Shape
    , position : ( Int, Int )
    }


type Shape
    = Circle


initialGame : Game
initialGame =
    { current =
        { shape = Circle
        , position = ( -1, 9 )
        }
    , level = 1
    }



-- UPDATE


tickGame : Float -> Game -> Game
tickGame delta game =
    game



-- VIEW


view : Game -> Html msg
view game =
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
            , currentView game
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


currentView : Game -> Collage.Form
currentView game =
    let
        ( posX, posY ) =
            game.current.position

        x =
            toFloat (posX * cellSize) + (cellSize / 2)

        y =
            toFloat (posY * cellSize) + (cellSize / 2)
    in
    Collage.rect cellSize cellSize
        |> Collage.filled (Color.rgb 255 255 0)
        |> Collage.move ( x, y )
