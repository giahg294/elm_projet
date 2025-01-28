<<<<<<< HEAD
module Drawing exposing (drawTcTurtle)

{-|
Le module de dessin fait
convertir le programme TcTurtle analysé en un graphique SVG.
-}

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Parsing exposing (Instruction)


type alias Point =
    { x : Float, y : Float }

drawTcTurtle : List Instruction -> Svg msg
drawTcTurtle program =
    let
        initialPosition = { x = 250, y = 250 } -- position initiale (250, 250)
        initialAngle = 0 -- angle initial
        commands = executeProgram program initialPosition initialAngle
    in
    svg [ width "500", height "500", viewBox "0 0 500 500", style "border: 1px solid black;" ]
        (List.map drawLine commands)

executeProgram : List Instruction -> Point -> Float -> List (Point, Point)
executeProgram instructions startPosition startAngle =
    -- 递归执行绘图指令的逻辑，返回一组点对
    -- exécuter de manière récursive les instructions de dessin et retourner un ensemble de paires de points.
    []

drawLine : (Point, Point) -> Svg msg
drawLine (start, end) =
    line
        [ x1 (String.fromFloat start.x)
        , y1 (String.fromFloat start.y)
        , x2 (String.fromFloat end.x)
        , y2 (String.fromFloat end.y)
        , stroke "black"
        , strokeWidth "2"
        ]
        []
=======
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
>>>>>>> c58020cbc6de7eb5daf99b31bbae3c8d53e47213
