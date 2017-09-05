module Input exposing (..)

-- MODEL


type alias Input =
    { left : Bool
    , right : Bool
    , debounces : List Debounce
    }


defaultInput : Input
defaultInput =
    { left = False
    , right = False
    , debounces = []
    }


type alias Debounce =
    { current : Float
    , interval : Float
    , key : Key
    }


initialDebounce : Key -> Debounce
initialDebounce key =
    { current = 0.0
    , interval = 0
    , key = key
    }


removeDebounce : Key -> List Debounce -> List Debounce
removeDebounce key =
    List.filter (\d -> d.key /= key)


type Key
    = Left
    | Right


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


tickDebounce : Float -> Input -> ( List Key, Input )
tickDebounce diff input =
    let
        debounces =
            List.map (\d -> { d | current = d.current + diff }) input.debounces

        keysFired =
            List.filter (\d -> d.current > d.interval) debounces
                |> List.map .key
    in
    ( keysFired
    , { input
        | debounces =
            List.map
                (\d ->
                    if d.current > d.interval then
                        { d | current = d.current - d.interval, interval = 150 }
                    else
                        d
                )
                debounces
      }
    )
