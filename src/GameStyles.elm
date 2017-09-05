module GameStyles exposing (ID(..), css, ns)

import Css exposing (..)
import Css.Namespace exposing (namespace)


type ID
    = PlayField
    | GameContainer


ns : String
ns =
    "game"


css : Stylesheet
css =
    (stylesheet << namespace ns)
        [ id GameContainer
            [ displayFlex
            , backgroundColor (rgb 128 128 128)
            , padding (px 16)
            ]
        , id PlayField
            [ padding (px 16) ]
        ]
