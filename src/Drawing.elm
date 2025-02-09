module Drawing exposing (..)

import Parsing exposing (Instruction(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Basics exposing (degrees, cos, sin)

type alias Tcturtle = { x : Float, y : Float, orient : Float }

-- rajoute une ligne SVG entre les 2 Tcturtle données à la liste de SVG donnée, et continue l'exécution des étapes de dessin
goForward : List Instruction -> Tcturtle -> Tcturtle -> (List (Svg msg)) -> (Tcturtle, (List (Svg msg)))
goForward steps turt next_turt svg_final =
    res_svg steps next_turt ( List.append svg_final
        [ line [ x1 (String.fromFloat turt.x)
                , y1 (String.fromFloat turt.y)
                , x2 (String.fromFloat next_turt.x)
                , y2 (String.fromFloat next_turt.y)
                ] []
        ]
    )

repeatSteps : Int -> List Instruction -> Tcturtle -> List (Svg msg) -> (Tcturtle, List (Svg msg))
repeatSteps n steps turt svg_final =
    if n <= 0 then
        (turt, svg_final)
    else
        let
            (newTurt, newSvg) = res_svg steps turt svg_final
        in
        repeatSteps (n - 1) steps newTurt newSvg

-- comme res_svg, mais prend un tuple comme 2ème argument comme ça on peut lui donner en entrée la sortie de repeatSteps directement
repeat : List Instruction -> (Tcturtle, (List (Svg msg))) -> (Tcturtle, (List (Svg msg)))
repeat rest_of_steps (turt, final_svg) =
    res_svg rest_of_steps turt final_svg

-- à partir d'une liste d'étape à effectuer, d'une Tcturtle initiale et d'une liste de SVG donnée, renvoie la Tcturtle et la liste de SVG finale après l'exécution des étapes demandées
res_svg : List Instruction -> Tcturtle -> (List (Svg msg)) -> (Tcturtle, (List (Svg msg)))
res_svg steps turt final_svg =
    case steps of
        [] ->
            (turt, final_svg)

        (step :: rest_of_steps) ->
            case step of
                Forward long ->
                    goForward rest_of_steps turt
                        (Tcturtle (turt.x + cos (degrees turt.orient) * long)
                                  (turt.y + sin (degrees turt.orient) * long)
                                  turt.orient
                        )
                        final_svg

                Right changement ->
                    res_svg rest_of_steps (Tcturtle turt.x turt.y (turt.orient + changement)) final_svg

                Left changement ->
                    res_svg rest_of_steps (Tcturtle turt.x turt.y (turt.orient - changement)) final_svg

                Repeat nb repeat_steps ->
                    repeat rest_of_steps (repeatSteps nb repeat_steps turt final_svg)