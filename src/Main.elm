module Main exposing (main)

import Html exposing (..)
import Html.CssHelpers
import MainStyles


-- PROGRAM


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


init : ( Model, Cmd msg )
init =
    ( initialModel
    , Cmd.none
    )



-- MODEL


type alias Model =
    {}


initialModel : Model
initialModel =
    {}



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- CSS HELEPERS


{ id, class, classList } =
    Html.CssHelpers.withNamespace MainStyles.ns



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "Hello, Elm!" ]
