module Input exposing (..)

-- MODEL


type alias Input =
    { left : Bool
    , right : Bool
    }


defaultInput : Input
defaultInput =
    { left = False
    , right = False
    }


type Key
    = Left
    | Right


handleKeyDown : Input -> Key -> Input
handleKeyDown input key =
    case key of
        Left ->
            { input | left = True }

        Right ->
            { input | right = True }


handleKeyUp : Input -> Key -> Input
handleKeyUp input key =
    case key of
        Left ->
            { input | left = False }

        Right ->
            { input | right = False }
