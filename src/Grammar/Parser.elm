module Grammar.Parser exposing (parse)

import Dict
import Grammar.Internal exposing (Grammar, Strategy(..))
import List.Extra as List
import NonemptyList exposing (NonemptyList)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra as Parser
import Pratt
import Set


parse : String -> Result (List Parser.DeadEnd) Grammar
parse string =
    Parser.run parser string


parser : Parser Grammar
parser =
    Parser.succeed rulesToGrammar
        |= (Parser.sequence
                { start = ""
                , separator = "\n"
                , end = ""
                , spaces = spacesOnly
                , item = rule
                , trailing = Parser.Optional
                }
                |> Parser.andThen (NonemptyList.fromList >> Parser.fromMaybe "List was empty")
           )


spacesOnly : Parser ()
spacesOnly =
    Parser.chompWhile (\c -> c == ' ')


rule : Parser ( String, Strategy )
rule =
    Parser.succeed Tuple.pair
        |. Parser.spaces
        |= tag
        |. Parser.spaces
        |. Parser.symbol "->"
        |. Parser.spaces
        |= strategy


strategy : Parser Strategy
strategy =
    Pratt.expression
        { oneOf =
            [ Pratt.literal <| Parser.map Literal literal
            , Pratt.literal <| Parser.map Tag tag
            , hidden
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 2 spacesOnly Concatenation
            , Pratt.infixLeft 1 (Parser.symbol "|") Alternation
            ]
        , spaces = spacesOnly
        }


hidden : Pratt.Config Strategy -> Parser Strategy
hidden config =
    Parser.succeed Hidden
        |. Parser.symbol "<"
        |. spacesOnly
        |= Pratt.subExpression 0 config
        |. spacesOnly
        |. Parser.symbol ">"


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


rulesToGrammar : NonemptyList ( String, Strategy ) -> Grammar
rulesToGrammar rules =
    let
        start : String
        start =
            rules
                |> NonemptyList.head
                |> Tuple.first

        groupedRules : List ( String, NonemptyList Strategy )
        groupedRules =
            rules
                |> NonemptyList.toList
                |> List.sortBy Tuple.first
                |> List.gatherEqualsBy Tuple.first
                |> List.map
                    (\( ( tag_, strategy1 ), grouped ) ->
                        let
                            restOfStrategies =
                                List.map Tuple.second grouped
                        in
                        ( tag_, NonemptyList.fromCons strategy1 restOfStrategies )
                    )
    in
    { start = start
    , rules = Dict.fromList groupedRules
    }
