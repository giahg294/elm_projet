module TcParser exposing (..)

import Parser exposing (..)

-- Define parsers for different instructions
forwardParser : Parser Instruction
forwardParser =
    map Forward
        (token "Forward" *> spaces *> int)

leftParser : Parser Instruction
leftParser =
    map Left
        (token "Left" *> spaces *> int)

rightParser : Parser Instruction
rightParser =
    map Right
        (token "Right" *> spaces *> int)

repeatParser : Parser Instruction
repeatParser =
    map Repeat
        (token "Repeat" *> spaces *> int)

-- Combine parsers using andThen and map
instructionParser : Parser Instruction
instructionParser =
    oneOf [ forwardParser, leftParser, rightParser, repeatParser ]

-- Your main parser to parse a list of instructions

programParser : Parser (List Instruction)
programParser =
    between (symbol "[") (symbol "]") (instructionParser sepBy (symbol ","))
