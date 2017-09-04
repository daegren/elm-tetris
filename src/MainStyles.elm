module MainStyles exposing (css, ns)

import Css exposing (..)
import Css.Namespace exposing (namespace)


ns : String
ns =
    "main"


css : Stylesheet
css =
    (stylesheet << namespace ns)
        [-- Shared Styles go here
        ]
