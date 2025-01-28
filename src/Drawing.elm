module Drawing exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

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
executeInstructions : List Instruction -> (Int, Int) -> Int -> List (Int, Int, Int, Int)
executeInstructions instructions (x, y) angle =
    case instructions of
        [] -> []
        Forward n :: rest ->
            let
                newX = x + (n * cos angle)
                newY = y + (n * sin angle)
            in
            (x, y, newX, newY) :: executeInstructions rest (newX, newY) angle
        Left n :: rest ->
            executeInstructions rest (x, y) (angle - n)
        Right n :: rest ->
            executeInstructions rest (x, y) (angle + n)
        Repeat _ _ :: rest -> 
            executeInstructions rest (x, y) angle

-- Fonction pour afficher une ligne SVG
viewLine : (Int, Int, Int, Int) -> Svg msg
viewLine (x1, y1, x2, y2) =
    line [ x1 (String.fromInt x1), y1 (String.fromInt y1), x2 (String.fromInt x2), y2 (String.fromInt y2), stroke "black", strokeWidth "2" ]
