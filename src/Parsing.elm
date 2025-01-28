module Parsing exposing (parseTcTurtle)
import Parser exposing (Parser, (|.), (|=), int, keyword, spaces, succeed, run, sepBy, symbol)

import List exposing (map)

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

-- 自定义的 between 函数
between : Parser a -> Parser b -> Parser c -> Parser c
between left right parser =
    left
        |. parser
        |. right

-- 程序解析器
programParser : Parser Program
programParser =
    between (symbol "[") (symbol "]") (sepBy instructionParser (symbol ","))

-- 指令解析器
instructionParser : Parser Instruction
instructionParser =
    oneOf
        [ keyword "Forward" |. spaces |= int |> List.map Forward
        , keyword "Left" |. spaces |= int |> List.map Left
        , keyword "Right" |. spaces |= int |> List.map Right
        , keyword "Repeat" |. spaces |= int |= (between (symbol "[") (symbol "]") (sepBy instructionParser (symbol ",")))
            |> List.map Repeat
        ]
