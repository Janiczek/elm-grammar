module Grammar.Parser exposing (parse)

import Grammar.Internal exposing (Chunk(..), Grammar, Rule)
import Parser exposing ((|.), (|=), Parser)
import Set


parse : String -> Result (List Parser.DeadEnd) Grammar
parse string =
    Parser.run parser string


parser : Parser Grammar
parser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = spacesOnly
        , item = rule
        , trailing = Parser.Optional
        }


spacesOnly : Parser ()
spacesOnly =
    Parser.chompWhile (\c -> c == ' ')


rule : Parser Rule
rule =
    Parser.succeed Rule
        |. Parser.spaces
        |= tag
        |. Parser.spaces
        |. Parser.symbol "->"
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = spacesOnly
            , item = chunk
            , trailing = Parser.Optional
            }


chunk : Parser Chunk
chunk =
    Parser.oneOf
        [ Parser.map Literal literal
        , Parser.map Tag tag
        ]


literal : Parser String
literal =
    Parser.succeed identity
        |. Parser.token "\""
        |= Parser.loop [] literalHelp


literalHelp : List String -> Parser (Parser.Step (List String) String)
literalHelp revStrs =
    Parser.oneOf
        [ Parser.token "\""
            |> Parser.map (\_ -> Parser.Done (String.join "" (List.reverse revStrs)))
        , Parser.chompWhile isUninterestingToLiteral
            |> Parser.getChompedString
            |> Parser.map (\str -> Parser.Loop (str :: revStrs))
        ]


isUninterestingToLiteral : Char -> Bool
isUninterestingToLiteral char =
    char /= '"'


tag : Parser String
tag =
    Parser.variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }
