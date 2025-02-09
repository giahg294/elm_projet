module Main exposing (..)

import Parsing exposing (..)
import Parser exposing (run)
import Browser
import Html exposing (Html, button, div, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Drawing exposing (..)
-- import Degbug

-- MAIN
main =
    Browser.element {init = init, update = update, view = view, subscriptions = subscriptions }

-- MODEL
type alias Model =
    { commandes : List Instruction
    , commande_str : String
    , commande_svg : List (Svg Msg)
    , erreur : Erreur
    , size : Float
    , x_i : Float
    , y_i : Float
    , couleur : String
-- , debugMsg : String  -- Ajout du champ debug
    }

type Erreur
    = None
    | Message String

init : () -> (Model, Cmd msg )
init _ =
    ( {commande_str = ""
    , commandes = []
    , commande_svg = []
    , erreur = None
    , size = 1
    , x_i = 0
    , y_i = 0
    , couleur = "black"
    -- , debugMsg = ""
    }, 
    
    Cmd.none)

-- UPDATE

type Msg
    = Change String
    | Draw 
    | Choix_couleur String 

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
            let 
                chemin = unwrap (run liste_instructions model.commande_str)
                resultatSvg = Drawing.res_svg chemin (Positions_turtle (model.x_i + 150.0) (model.y_i + 150.0) 0 model.couleur) []
                -- debugText = "Resultat res_svg : " ++ Debug.toString resultatSvg
            in
            if List.isEmpty chemin then
                ({ model
                    | commandes = []
                    , erreur = Message "Commande invalide"
                    -- , debugMsg = "Commande invalide"
                }, Cmd.none)
            else
                ({ model
                    | commandes = chemin
                    , erreur = None
                    , commande_svg = Tuple.second resultatSvg
                    -- , debugMsg = debugText  --
                }, Cmd.none)
       
        Choix_couleur color ->  -- Changer la couleur
            ({ model | couleur = color }, Cmd.none)
            

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- VIEW
view : Model -> Html Msg
view model =
    div 
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "background-image" "url('image_tortue.jpg')"  -- Spécification de l'image en fond
        , Html.Attributes.style "background-size" "cover"   -- Cette option ajuste la taille de l'image pour couvrir l'arrière-plan
        , Html.Attributes.style "background-position" "center"  -- Centrer l'image
        , Html.Attributes.style "height" "100vh"  -- S'assurer que l'image couvre toute la hauteur de la fenêtre
        , Html.Attributes.style "width" "100%"  -- S'assurer que l'image couvre toute la largeur
        ]
        [ Html.h1 [ Html.Attributes.style "color" "red" ] [ Html.text "TcTurtle Project" ]
        , Html.h2 [ Html.Attributes.style "color" "black" ] [ Html.text "Type in your code below:" ]
        , div [ Html.Attributes.style "margin" "10px" ]
            [ input [ placeholder "ex: [Forward 10]", value model.commande_str, onInput Change] []
            ]
        , div [ Html.Attributes.style "margin" "10px" ]
            [ button [ onClick Draw, Html.Attributes.style "padding" "5px"] [ Html.text "Draw <3" ]
            ]
        , div [ Html.Attributes.style "margin" "10px" ]
            [ button [ onClick (Choix_couleur "black"), Html.Attributes.style "padding" "5px", Html.Attributes.style "background-color" "black",  Html.Attributes.style "color" "white"] [ Html.text "Noir"]
            , button [ onClick (Choix_couleur "red"), Html.Attributes.style "padding" "5px", Html.Attributes.style "background-color" "red" ] [ Html.text "Rouge" ]
            , button [ onClick (Choix_couleur "green"), Html.Attributes.style "padding" "5px", Html.Attributes.style "background-color" "green" ] [ Html.text "Vert" ]
            , button [ onClick (Choix_couleur "blue"), Html.Attributes.style "padding" "5px", Html.Attributes.style "background-color" "blue" ] [ Html.text "Bleu" ]
            , button [ onClick (Choix_couleur "pink"), Html.Attributes.style "padding" "5px", Html.Attributes.style "background-color" "#FFB6C1" ] [ Html.text "Rose Pâle" ]
            , button [ onClick (Choix_couleur "fuchsia"), Html.Attributes.style "padding" "5px", Html.Attributes.style "background-color" "#FF00FF" ] [ Html.text "Rose Barbie" ]
            , button [ onClick (Choix_couleur "purple"), Html.Attributes.style "padding" "5px", Html.Attributes.style "background-color" "#9B4D96" ] [ Html.text "Violet" ]
            , button [ onClick (Choix_couleur "yellow"), Html.Attributes.style "padding" "5px", Html.Attributes.style "background-color" "yellow" ] [ Html.text "Jaune" ]
            ]
        , case model.erreur of
            None -> 
                div [ Html.Attributes.style "margin" "10px", Html.Attributes.style "border" "1px solid #ccc", Html.Attributes.style "padding" "10px" ]
                    [ svg [ Svg.Attributes.width "300", Svg.Attributes.height "300", viewBox "0 0 300 300" ] model.commande_svg ]
            Message msg ->
                div [ Html.Attributes.style "color" "black", Html.Attributes.style "text-align" "left", Html.Attributes.style "width" "300px" ]
                    [ Html.h2 [] [ Html.text "Instructions invalides" ] ]
        ]