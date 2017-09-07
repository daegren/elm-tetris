module Main exposing (main)

import AnimationFrame
import Game
import Html exposing (..)
import Html.CssHelpers
import Input
import Keyboard
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
    { game : Game.Game
    , input : Input.Input
    }


initialModel : Model
initialModel =
    { game = Game.initialGame
    , input = Input.defaultInput
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]



-- UPDATE


type Msg
    = NoOp
    | Tick Time.Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            let
                ( keys, input ) =
                    Input.tickDebounce time model.input
            in
            ( { model | game = Game.tickGame time keys model.game, input = input }, Cmd.none )

        KeyDown key ->
            let
                input =
                    Maybe.map (Input.handleKeyDown model.input) (mapKey key)
            in
            case input of
                Just i ->
                    ( { model | input = i }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        KeyUp key ->
            let
                input =
                    Maybe.map (Input.handleKeyUp model.input) (mapKey key)
            in
            case input of
                Just i ->
                    ( { model | input = i }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


mapKey : Keyboard.KeyCode -> Maybe Input.Key
mapKey keyCode =
    case keyCode of
        -- Left Arrow
        37 ->
            Just Input.Left

        -- Right arrow
        39 ->
            Just Input.Right

        -- c
        67 ->
            Just Input.RotateClockwise

        _ ->
            let
                _ =
                    Debug.log "mapKey" keyCode
            in
            Nothing



-- CSS HELEPERS


{ id, class, classList } =
    Html.CssHelpers.withNamespace MainStyles.ns



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Game.view model.game ]
