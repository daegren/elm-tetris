module TileBag exposing (TileBag, init, pull)

import Random
import Tetromino exposing (Shape(..))


type TileBag
    = TileBag
        { available : List Shape
        , seed : Random.Seed
        }


init : Random.Seed -> TileBag
init seed =
    TileBag
        { available = fullBag
        , seed = seed
        }


pull : TileBag -> ( Shape, TileBag )
pull ((TileBag { available, seed }) as tileBag) =
    let
        ( shape, seed0 ) =
            Random.step (nextPiece available) seed

        bag =
            List.filter (\s -> s /= shape) available
    in
    ( shape
    , TileBag
        { available =
            if List.isEmpty bag then
                fullBag
            else
                bag
        , seed = seed0
        }
    )


fullBag : List Shape
fullBag =
    [ O, T, I, S, Z, J, L ]



-- GENERATORS


nextPiece : List Shape -> Random.Generator Shape
nextPiece available =
    Random.map (Maybe.withDefault O) (oneOf available)


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
