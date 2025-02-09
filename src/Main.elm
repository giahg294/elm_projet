module Main exposing (..)

import Parsing exposing (..)
import Parser exposing (run)
import Browser
import Html exposing (Html, button, div, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (viewBox)
import Drawing exposing (..)
import Time exposing (..)
import Process
import Task

-- MAIN
main =
    Browser.element {init = init, update = update, view = view, subscriptions = subscriptions }

-- MODEL
type alias Model =
    { commandes : List Instruction
    , commande_str : String
    , erreur : Erreur
    , taille_dessin : Float
    , initial_x : Float
    , initial_y : Float
    , commandesExecutees : List Instruction
    , drawing : Bool
    , svgPartiel : List (Svg Msg)
    , svgFini : List (Svg Msg)
    }

type Erreur
    = None
    | Message String

init : () -> (Model, Cmd msg )
init _ =
    ( {commande_str = "", commandes = [], erreur = None, commandesExecutees = [], drawing = False, svgPartiel = [], svgFini = [], taille_dessin = 1, initial_x = 0, initial_y = 0}, Cmd.none )

-- UPDATE

type Msg
    = Change String
    | Draw
    | Timer

unwrap : Result (List Parser.DeadEnd) (List Instruction) -> List Instruction
unwrap res =
    case res of
        Ok cool ->
            cool

        Err _ ->
            []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Change str ->
            ({ model | commande_str = str }, Cmd.none)

        Draw ->
            let chemin = unwrap (run liste_instructions model.commande_str) in
            if List.isEmpty chemin then
                ({ model
                    | commandes = []
                    , erreur = Message "Commande invalide"
                }, Cmd.none)
            else
                ({ model
                    | commandes = chemin
                    , erreur = None
                    , drawing = True
                    , svgFini = (Tuple.second (Drawing.res_svg chemin  (Tcturtle (model.initial_x + 150.0) (model.initial_y + 150.0) 0) []))
                }, Cmd.none)
        
        Timer ->
            if model.drawing then
                case model.svgFini of
                    [] ->
                        ( { model
                            | drawing = False
                            , commandesExecutees = []
                          }
                        , Cmd.none
                        )
                    nextCommand :: remaining ->
                        ( { model
                            |svgFini = remaining, 
                            svgPartiel = model.svgPartiel ++ [nextCommand]
                          }
                        , Task.perform (\_ -> Timer) (Process.sleep 5)
                        )
            else
                (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- VIEW
view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column", Html.Attributes.style "align-items" "center", Html.Attributes.style "justify-content" "center"]
        [ Html.h1 [ Html.Attributes.style "color" "red" ] [ Html.text "TcTurtle Project" ]
        , Html.h2 [ Html.Attributes.style "color" "black" ] [ Html.text "Type in your code below:" ]
        , div [ Html.Attributes.style "margin" "10px" ]
            [ input [ placeholder "ex: [Forward 10]", value model.commande_str, onInput Change] []
            ]
        , div [ Html.Attributes.style "margin" "10px" ]
            [ button [ onClick Draw, Html.Attributes.style "padding" "5px"] [ Html.text "Appuyez ici pour afficher les instructions disponibles ou exécuter votre code " ]
            ]
        , case model.erreur of
            None ->
                div [ Html.Attributes.style "margin" "10px", Html.Attributes.style "border" "1px solid #ccc", Html.Attributes.style "padding" "10px" ]
                    [ svg [ Svg.Attributes.width (String.fromInt 300), Svg.Attributes.height (String.fromInt 300), viewBox "0 0 300 300" ]
                        model.svgPartiel
                    ]
            Message msg ->
                div [ Html.Attributes.style "color" "black", Html.Attributes.style "text-align" "left", Html.Attributes.style "width" "300px" ]
                    [ Html.h2 [] [ Html.text "Instructions invalides" ]]
        ]