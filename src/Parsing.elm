module Parsing exposing (parseTcTurtle)
import Parser exposing (Parser, (|.), (|=), int, keyword, spaces, succeed, run, sepBy)
import Parser.Advanced exposing (between)
import List exposing (map as listMap)

{-|
Le module de parsing est chargé d'analyser le programme TcTurtle saisi par l'utilisateur 
et de le convertir en une structure de données Elm structurée.
-}


-- Structure des donnees
type Instruction
    = Forward Int
    | Left Int
    | Right Int
    | Repeat Int (List Instruction)

type Program =
    List Instruction

-- Parser
parseTcTurtle : String -> Result String Program
parseTcTurtle input =
    run programParser input

programParser : Parser Program
programParser =
    between (symbol "[") (symbol "]") (sepBy instructionParser (symbol ","))

instructionParser : Parser Instruction
instructionParser =
    oneOf
        [ keyword "Forward" |. spaces |= int |> listMap Forward
        , keyword "Left" |. spaces |= int |> listMap Left
        , keyword "Right" |. spaces |= int |> listMap Right
        , keyword "Repeat" |. spaces |= int |= (between (symbol "[") (symbol "]") (sepBy instructionParser (symbol ",")))
            |> listMap Repeat
        ]
