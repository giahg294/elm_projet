module Drawing exposing (..) 

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Basics exposing (degrees, cos, sin)

-- Définir les instructions de dessin
type Instruction
    = Forward Int
    | Left Int
    | Right Int
    | Repeat Int (List Instruction)

-- La fonction pour afficher un dessin à partir d'une liste d'instructions
viewDrawing : List Instruction -> Svg msg
viewDrawing instructions =
    let
        -- Calculer les positions et les éléments à dessiner
        lines = executeInstructions instructions (0, 0) 0
    in
    svg [ width "500", height "500" ]
        (List.map viewLine lines)

-- Exécuter les instructions pour générer des lignes SVG
executeInstructions : List Instruction -> (Float, Float) -> Float -> List (Float, Float, Float, Float)
executeInstructions instructions (x, y) angle =
    case instructions of
        [] -> []
        Forward n :: rest ->
            let
                radianAngle = degrees angle
                newX = x + toFloat n * cos radianAngle
                newY = y + toFloat n * sin radianAngle
            in
            (x, y, newX, newY) :: executeInstructions rest (newX, newY) angle
        Left n :: rest ->
            executeInstructions rest (x, y) (angle - toFloat n)
        Right n :: rest ->
            executeInstructions rest (x, y) (angle + toFloat n)
        Repeat n subInstructions :: rest ->
            let
                repeated = List.concat (List.repeat n subInstructions)
            in
            executeInstructions (repeated ++ rest) (x, y) angle

-- Fonction pour afficher une ligne SVG
viewLine : (Float, Float, Float, Float) -> Svg msg
viewLine (x1, y1, x2, y2) =
    line 
        [ Svg.Attributes.x1 (String.fromFloat x1)
        , Svg.Attributes.y1 (String.fromFloat y1)
        , Svg.Attributes.x2 (String.fromFloat x2)
        , Svg.Attributes.y2 (String.fromFloat y2)
        , stroke "black"
        , strokeWidth "2"
        ]
        []
