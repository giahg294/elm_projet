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
