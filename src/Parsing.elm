module Parsing exposing (parseTcTurtle)


{-|
Le module de parsing est chargé d'analyser le programme TcTurtle saisi par l'utilisateur 
et de le convertir en une structure de données Elm structurée.
-}

import Parser exposing (Parser, (|.), (|=), int, keyword, spaces, succeed, run)

-- 数据结构
type Instruction
    = Forward Int
    | Left Int
    | Right Int
    | Repeat Int (List Instruction)

type Program =
    List Instruction

-- 解析器
parseTcTurtle : String -> Result String Program
parseTcTurtle input =
    run programParser input

programParser : Parser Program
programParser =
    between (symbol "[") (symbol "]") (sepBy instructionParser (symbol ","))

instructionParser : Parser Instruction
instructionParser =
    oneOf
        [ keyword "Forward" |. spaces |= int |> map Forward
        , keyword "Left" |. spaces |= int |> map Left
        , keyword "Right" |. spaces |= int |> map Right
        , keyword "Repeat" |. spaces |= int |= (between (symbol "[") (symbol "]") (sepBy instructionParser (symbol ",")))
            |> map Repeat
        ]
