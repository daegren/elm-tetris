module Main exposing (main)

import AnimationFrame
import Game
import Html exposing (..)
import Html.CssHelpers
import MainStyles
import Time


-- PROGRAM


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd msg )
init =
    ( initialModel
    , Cmd.none
    )



-- MODEL


type alias Model =
    { game : Game.Game }


initialModel : Model
initialModel =
    { game = Game.initialGame }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



-- UPDATE


type Msg
    = NoOp
    | Tick Time.Time


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            ( { model | game = Game.tickGame time model.game }, Cmd.none )



-- CSS HELEPERS


{ id, class, classList } =
    Html.CssHelpers.withNamespace MainStyles.ns



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Game.view model.game ]
