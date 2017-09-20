module GameStyles exposing (Class(..), ID(..), css, ns)

import Css exposing (..)
import Css.Elements exposing (button)
import Css.Namespace exposing (namespace)


type ID
    = PlayField
    | GameContainer
    | Overlay


type Class
    = Title
    | Actions
    | Details


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
            , position relative
            ]
        , id PlayField
            [ padding (px 16) ]
        , id Overlay
            [ position absolute
            , top zero
            , right zero
            , left zero
            , bottom zero
            , backgroundColor (rgba 255 255 255 0.75)
            , zIndex (int 1)
            , displayFlex
            , alignItems center
            , flexDirection column
            , children
                [ class Title
                    [ fontSize (em 3)
                    , padding2 (em 1.5) zero
                    , flex (num 1)
                    ]
                , class Actions
                    [ flex (num 1)
                    , children
                        [ button [ fontSize (em 1.5) ]
                        ]
                    ]
                , class Details
                    [ flex (num 1)
                    , fontSize (em 1.5)
                    ]
                ]
            ]
        ]
