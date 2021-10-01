module Grammar.Parser exposing (Context(..), Parser, Problem(..), parse)

import Dict
import Grammar.Internal exposing (Grammar, Strategy(..))
import List.Extra as List
import NonemptyList exposing (NonemptyList)
import Parser.Advanced as Parser exposing ((|.), (|=), DeadEnd)
import Parser.Extra as Parser
import Pratt.Advanced as Pratt
import Set


type alias Parser a =
    Parser.Parser Context Problem a


type Context
    = InLiteral
    | InHidden
    | InGrouped
    | InTag
    | InStrategy
    | InRule


type Problem
    = RuleListWasEmpty
    | ShouldntHappen
    | CouldntFindRuleOnLeftSide String
    | CouldntFindStartingRuleOnLeftSide String
    | ExpectingTagName
    | ExpectingOpeningDoubleQuote
    | ExpectingClosingDoubleQuote
    | ExpectingLeftAngleBracket
    | ExpectingRightAngleBracket
    | ExpectingLeftParenthesis
    | ExpectingRightParenthesis
    | ExpectingPipe
    | ExpectingArrow
    | ExpectingNewline
    | ExpectingLiteral String
    | ExpectingPlusSign
    | ExpectingAsterisk


parse : String -> Result (List (DeadEnd Context Problem)) Grammar
parse string =
    Parser.run parser string


parser : Parser Grammar
parser =
    Parser.succeed rulesToGrammar
        |= (Parser.sequence
                { start = empty
                , separator = Parser.Token "\n" ExpectingNewline
                , end = empty
                , spaces = spacesOnly
                , item = rule
                , trailing = Parser.Optional
                }
                |> Parser.andThen (NonemptyList.fromList >> Parser.fromMaybe RuleListWasEmpty)
           )


empty : Parser.Token Problem
empty =
    Parser.Token "" ShouldntHappen


spacesOnly : Parser ()
spacesOnly =
    Parser.chompWhile (\c -> c == ' ')


rule : Parser ( String, Strategy )
rule =
    Parser.succeed Tuple.pair
        |. Parser.spaces
        |= tag
        |. Parser.spaces
        |. Parser.token (Parser.Token "->" ExpectingArrow)
        |. Parser.spaces
        |= strategy
        |> Parser.inContext InRule


strategy : Parser Strategy
strategy =
    Pratt.expression
        { oneOf =
            [ hidden
            , grouped
            , Pratt.literal <| Parser.map Literal literal
            , Pratt.literal <| Parser.map Tag tag
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 1 (Parser.token (Parser.Token "|" ExpectingPipe)) Alternation
            , Pratt.infixLeft 2 spacesOnly Concatenation
            , Pratt.postfix 3 (Parser.token (Parser.Token "+" ExpectingPlusSign)) OneOrMore
            , Pratt.postfix 4 (Parser.token (Parser.Token "*" ExpectingAsterisk)) ZeroOrMore
            ]
        , spaces = spacesOnly
        }
        |> Parser.inContext InStrategy


hidden : Pratt.Config Context Problem Strategy -> Parser Strategy
hidden config =
    Parser.succeed Hidden
        |. Parser.token (Parser.Token "<" ExpectingLeftAngleBracket)
        |. spacesOnly
        |= Pratt.subExpression 0 config
        |. spacesOnly
        |. Parser.token (Parser.Token ">" ExpectingRightAngleBracket)
        |> Parser.inContext InHidden


grouped : Pratt.Config Context Problem Strategy -> Parser Strategy
grouped config =
    Parser.succeed identity
        |. Parser.token (Parser.Token "(" ExpectingLeftParenthesis)
        |. spacesOnly
        |= Pratt.subExpression 0 config
        |. spacesOnly
        |. Parser.token (Parser.Token ")" ExpectingRightParenthesis)
        |> Parser.inContext InGrouped


literal : Parser String
literal =
    Parser.succeed identity
        |. Parser.token (Parser.Token "\"" ExpectingOpeningDoubleQuote)
        |= Parser.loop [] literalHelp
        |> Parser.inContext InLiteral


literalHelp : List String -> Parser (Parser.Step (List String) String)
literalHelp revStrs =
    Parser.oneOf
        [ Parser.token (Parser.Token "\"" ExpectingClosingDoubleQuote)
            |> Parser.map (\_ -> Parser.Done (String.join "" (List.reverse revStrs)))
        , Parser.chompWhile isUninterestingToLiteral
            |> Parser.getChompedString
            |> Parser.andThen
                (\str ->
                    if String.isEmpty str then
                        Parser.problem ExpectingClosingDoubleQuote

                    else
                        Parser.succeed str
                )
            |> Parser.map (\str -> Parser.Loop (str :: revStrs))
        ]


isUninterestingToLiteral : Char -> Bool
isUninterestingToLiteral char =
    char /= '"'


tag : Parser String
tag =
    Parser.variable
        { expecting = ExpectingTagName
        , start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }
        |> Parser.inContext InTag


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
                    (\( ( tag_, strategy1 ), grouped_ ) ->
                        let
                            restOfStrategies =
                                List.map Tuple.second grouped_
                        in
                        ( tag_, NonemptyList.fromCons strategy1 restOfStrategies )
                    )
    in
    { start = start
    , rules = Dict.fromList groupedRules
    }
