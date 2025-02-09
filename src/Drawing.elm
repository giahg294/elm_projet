module Drawing exposing (..)

import Parsing exposing (Instruction(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Basics exposing (degrees, cos, sin)

type alias Positions_turtle = { x : Float, y : Float, orient : Float, couleur : String }

-- rajoute une ligne SVG entre les 2 positions_turtle données à la liste de SVG donnée, et continue l'exécution des étapes de dessin
avancer : List Instruction -> Positions_turtle -> Positions_turtle -> (List (Svg msg)) -> (Positions_turtle, (List (Svg msg)))
avancer etapes positions_turtle next_positions_turtle svg_final =
    res_svg etapes next_positions_turtle ( List.append svg_final
        [ line [ x1 (String.fromFloat positions_turtle.x)
                , y1 (String.fromFloat positions_turtle.y)
                , x2 (String.fromFloat next_positions_turtle.x)
                , y2 (String.fromFloat next_positions_turtle.y)
                , stroke positions_turtle.couleur   -- Correction ici pour rendre la ligne visible
                , strokeWidth "2"  -- Épaisseur du trait
                ] []
        ]

    )

repeatetapes : Int -> List Instruction -> Positions_turtle -> List (Svg msg) -> (Positions_turtle, List (Svg msg))
repeatetapes n etapes positions_turtle svg_final =
    if n <= 0 then
        (positions_turtle, svg_final)
    else
        let
            (new_positions_turtle, newSvg) = res_svg etapes positions_turtle svg_final
        in
        repeatetapes (n - 1) etapes new_positions_turtle newSvg

-- comme res_svg, mais prend un tuple comme 2ème argument comme ça on peut lui donner en entrée la sortie de repeatetapes directement
repeat : List Instruction -> (Positions_turtle, (List (Svg msg))) -> (Positions_turtle, (List (Svg msg)))
repeat rest_of_etapes (positions_turtle, final_svg) =
    res_svg rest_of_etapes positions_turtle final_svg

-- à partir d'une liste d'étape à effectuer, d'une positions_turtle initiale et d'une liste de SVG donnée, renvoie la positions_turtle et la liste de SVG finale après l'exécution des étapes demandées
res_svg : List Instruction -> Positions_turtle -> (List (Svg msg)) -> (Positions_turtle, (List (Svg msg)))
res_svg etapes positions_turtle final_svg =
    case etapes of
        [] ->
            (positions_turtle, final_svg)

        (step :: rest_of_etapes) ->
            case step of
                Forward long ->
                    avancer rest_of_etapes positions_turtle
                        (Positions_turtle (positions_turtle.x + cos (degrees positions_turtle.orient) * long)
                                  (positions_turtle.y + sin (degrees positions_turtle.orient) * long)
                                  positions_turtle.orient
                                  positions_turtle.couleur
                        )
                        final_svg

                Right changement ->
                    res_svg rest_of_etapes (Positions_turtle positions_turtle.x positions_turtle.y (positions_turtle.orient + changement) positions_turtle.couleur) final_svg

                Left changement ->
                    res_svg rest_of_etapes (Positions_turtle positions_turtle.x positions_turtle.y (positions_turtle.orient - changement) positions_turtle.couleur) final_svg

                Repeat nb repeat_etapes ->
                    repeat rest_of_etapes (repeatetapes nb repeat_etapes positions_turtle final_svg)