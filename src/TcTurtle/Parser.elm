module TcTurtle.Parser exposing (read, Instruction(..))

import Parser exposing (Parser, run, succeed, token, int, (|=), spaces, lazy, andThen, map, oneOf, loop)
import Parser as Parser exposing (Step(..))

-- Define Instruction Type
type Instruction
    = Forward Int
    | Left Int
    | Right Int
    | Repeat Int (List Instruction)


-- Read function: takes a string and parses it into TcTurtle instructions
read : String -> Result (List Parser.DeadEnd) (List Instruction)
read input =
    run programParser input


-- Top-level program parser: Parses a list of instructions inside brackets
programParser : Parser (List Instruction)
programParser =
    lazy (\_ ->
        succeed identity
            |> andThen (\_ -> token "[")
            |> andThen (\_ -> spaces)
            |> andThen (\_ -> instructionListParser)
            |> andThen (\instructions -> token "]" |> map (\_ -> instructions))
    )


-- Parses a list of instructions using `loop`
instructionListParser : Parser (List Instruction)
instructionListParser =
    loop [] instructionListStep

instructionListStep : List Instruction -> Parser (Parser.Step (List Instruction) (List Instruction))
instructionListStep acc =
    oneOf
        [ succeed (\instr -> Loop (acc ++ [instr]))  -- Keep looping with new instruction added
            |= instructionParser
        , succeed (Done acc)  -- Stop looping and return final list
        ]


-- Parses a single instruction
instructionParser : Parser Instruction
instructionParser =
    oneOf
        [ parseForward
        , parseLeft
        , parseRight
        , parseRepeat
        ]


-- Parses "Forward x"
parseForward : Parser Instruction
parseForward =
    succeed Forward
        |= (token "Forward" |> andThen (\_ -> spaces |> andThen (\_ -> int)))


-- Parses "Left x"
parseLeft : Parser Instruction
parseLeft =
    succeed Left
        |= (token "Left" |> andThen (\_ -> spaces |> andThen (\_ -> int)))


-- Parses "Right x"
parseRight : Parser Instruction
parseRight =
    succeed Right
        |= (token "Right" |> andThen (\_ -> spaces |> andThen (\_ -> int)))


-- Parses "Repeat x [ instructions ]"
parseRepeat : Parser Instruction
parseRepeat =
    succeed Repeat
        |= (token "Repeat" |> andThen (\_ -> spaces |> andThen (\_ -> int)))
        |= (token "[" |> andThen (\_ -> instructionListParser |> andThen (\lst -> token "]" |> map (\_ -> lst))))
