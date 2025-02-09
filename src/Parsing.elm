module Parsing exposing (..)

import Parser exposing (..)
import String exposing (..)

type Instruction
    = Forward Float
    | Right Float
    | Left Float
    | Repeat Int (List Instruction)

-- Pour parser une liste complète de commandes
extraire_instructions : Parser Instruction
extraire_instructions = oneOf [
    parseForward
    , parseLeft
    , parseRight
    , parseRepeat
    ]
liste_instructions : Parser (List Instruction)
liste_instructions = 
    Parser.sequence 
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = spaces
        , item = extraire_instructions
        , trailing = Optional
        }

-- Parses "Forward x"
parseForward : Parser Instruction
parseForward =
    succeed (\n -> Forward (Basics.toFloat n))
        |. symbol "Forward"
        |. spaces
        |= int



-- Parses "Left x"
parseLeft : Parser Instruction
parseLeft =
    succeed (\n -> Left (Basics.toFloat n))
        |. symbol "Left"
        |. spaces
        |= int



-- Parses "Right x"
parseRight : Parser Instruction
parseRight =
    succeed (\n -> Right (Basics.toFloat n))  -- Explicitly use Basics.toFloat
        |. symbol "Right"
        |. spaces
        |= int


-- Parses "Repeat x [ instructions ]"repeatParser : Parser Command
parseRepeat =
    succeed Repeat
        |. symbol "Repeat"
        |. spaces
        |= int
        |. spaces
        |. symbol "["
        |= lazy (\_ -> sequence { start = "", separator = ",", end = "", spaces = spaces, item = extraire_instructions, trailing = Parser.Forbidden }) -- Sans lazy, on aurait une récursion infinie car commandParser dépend de lui-même (pour les commandes imbriquées)
        |. symbol "]"

