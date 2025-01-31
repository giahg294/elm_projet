module TcTurtle.Drawing exposing (renderDrawing)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Basics exposing (degrees, cos, sin)
import TcTurtle.Parser exposing (..)

type alias Point =
    { x : Float
    , y : Float
    }

renderDrawing : List Instruction -> Svg msg
renderDrawing instructions =
    let
        -- Start at the center of the canvas
        startPoint = { x = 250, y = 250 }
        lines = executeInstructions instructions startPoint 0
    in
    svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        ]
        (List.map viewLine lines)

executeInstructions : List Instruction -> Point -> Float -> List (Point, Point)
executeInstructions instructions startPoint angle =
    case instructions of
        [] ->
            []

        Forward n :: rest ->
            let
                radians = degrees angle
                newPoint =
                    { x = startPoint.x + toFloat n * cos radians
                    , y = startPoint.y + toFloat n * sin radians
                    }
            in
            (startPoint, newPoint) :: executeInstructions rest newPoint angle

        Left n :: rest ->
            executeInstructions rest startPoint (angle - toFloat n)

        Right n :: rest ->
            executeInstructions rest startPoint (angle + toFloat n)

        Repeat times inner :: rest ->
            let
                repeatedLines =
                    List.concatMap
                        (\_ -> executeInstructions inner startPoint angle)
                        (List.range 1 times)
            in
            repeatedLines ++ executeInstructions rest startPoint angle

viewLine : (Point, Point) -> Svg msg
viewLine (startPoint, endPoint) =
    line
        [ x1 (String.fromFloat startPoint.x)
        , y1 (String.fromFloat startPoint.y)
        , x2 (String.fromFloat endPoint.x)
        , y2 (String.fromFloat endPoint.y)
        , stroke "black"
        , strokeWidth "2"
        ]
        []
