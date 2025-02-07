module Main exposing (..)

import Parser exposing (..)
import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import TcTurtle.Parser exposing (read, Instruction(..))
import TcTurtle.Drawing exposing (renderDrawing)

type alias Model =
    { input : String
    , result : Result (List Parser.DeadEnd) (List Instruction)
    }

type Msg
    = UpdateInput String
    | ParseInput

init : Model
init =
    { input = ""
    , result = Ok []
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput newInput ->
            { model | input = newInput }

        ParseInput ->
            { model | result = read model.input }

view : Model -> Html Msg
view model =
    div []
        [ input
            [ value model.input
            , onInput UpdateInput
            , placeholder "Enter TcTurtle commands here..."
            ]
            []
        , button [ onClick ParseInput ] [ text "Render" ]
        , case model.result of
            Err _ ->
                div [] [ text "Invalid commands!" ]

            Ok commands ->
                renderDrawing commands
        ]

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

main =
    Browser.sandbox { init = init, update = update, view = view }