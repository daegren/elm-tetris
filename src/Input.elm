module Input exposing (..)


type alias Input =
    { left : Bool
    , right : Bool
    , hardDrop : Bool
    , rotation : Maybe Rotation
    , debounces : List Debounce
    }


defaultInput : Input
defaultInput =
    { left = False
    , right = False
    , hardDrop = False
    , rotation = Nothing
    , debounces = []
    }


type alias Debounce =
    { current : Float
    , interval : Maybe Float
    , key : Key
    }


initialDebounce : Key -> Debounce
initialDebounce key =
    { current = 0.0
    , interval = Just 0
    , key = key
    }


removeDebounce : Key -> List Debounce -> List Debounce
removeDebounce key =
    List.filter (\d -> d.key /= key)


type Rotation
    = Clockwise
    | CounterClockwise


type Key
    = Left
    | Right
    | RotateClockwise
    | RotateCounterClockwise
    | HardDrop


handleKeyDown : Input -> Key -> Input
handleKeyDown input key =
    case key of
        Left ->
            if not input.left then
                { input | left = True, debounces = initialDebounce Left :: input.debounces }
            else
                input

        Right ->
            if not input.right then
                { input | right = True, debounces = initialDebounce Right :: input.debounces }
            else
                input

        RotateClockwise ->
            case input.rotation of
                Just rotation ->
                    input

                Nothing ->
                    { input | rotation = Just Clockwise, debounces = initialDebounce RotateClockwise :: input.debounces }

        RotateCounterClockwise ->
            case input.rotation of
                Just rotation ->
                    input

                Nothing ->
                    { input | rotation = Just CounterClockwise, debounces = initialDebounce RotateCounterClockwise :: input.debounces }

        HardDrop ->
            if not input.hardDrop then
                { input | hardDrop = True, debounces = initialDebounce HardDrop :: input.debounces }
            else
                input


handleKeyUp : Input -> Key -> Input
handleKeyUp input key =
    case key of
        Left ->
            if input.left then
                { input | left = False, debounces = removeDebounce Left input.debounces }
            else
                input

        Right ->
            if input.right then
                { input | right = False, debounces = removeDebounce Right input.debounces }
            else
                input

        RotateClockwise ->
            case input.rotation of
                Just rotation ->
                    case rotation of
                        Clockwise ->
                            { input | rotation = Nothing, debounces = removeDebounce RotateClockwise input.debounces }

                        CounterClockwise ->
                            input

                Nothing ->
                    input

        RotateCounterClockwise ->
            case input.rotation of
                Just rotation ->
                    case rotation of
                        Clockwise ->
                            input

                        CounterClockwise ->
                            { input | rotation = Nothing, debounces = removeDebounce RotateCounterClockwise input.debounces }

                Nothing ->
                    input

        HardDrop ->
            if input.hardDrop then
                { input | hardDrop = False, debounces = removeDebounce HardDrop input.debounces }
            else
                input


tickDebounce : Float -> Input -> ( List Key, Input )
tickDebounce diff input =
    let
        debounces =
            List.map (\d -> { d | current = d.current + diff }) input.debounces

        keysFired =
            List.filter
                (\d ->
                    case d.interval of
                        Just interval ->
                            d.current > interval

                        Nothing ->
                            False
                )
                debounces
                |> List.map .key
    in
    ( keysFired
    , { input
        | debounces =
            List.map
                (\d ->
                    case d.interval of
                        Just interval ->
                            if d.current > interval then
                                case d.key of
                                    RotateClockwise ->
                                        { d | current = 0, interval = Nothing }

                                    RotateCounterClockwise ->
                                        { d | current = 0, interval = Nothing }

                                    Left ->
                                        { d | current = d.current - interval, interval = Just 150 }

                                    Right ->
                                        { d | current = d.current - interval, interval = Just 150 }

                                    HardDrop ->
                                        { d | current = 0, interval = Nothing }
                            else
                                d

                        Nothing ->
                            d
                )
                debounces
      }
    )
