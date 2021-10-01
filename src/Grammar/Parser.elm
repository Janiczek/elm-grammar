module Grammar.Parser exposing (Context(..), Parser, Problem(..), parse)

import Dict
import Grammar.Internal as Internal exposing (Grammar)
import Grammar.Strategy exposing (Strategy(..))
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
    | ExpectingQuestionMark
    | ExpectingEndOfString
    | ExpectingAmpersand
    | ExpectingLookahead


parse : String -> Result (List (DeadEnd Context Problem)) Grammar
parse string =
    Parser.run parser string


parser : Parser Grammar
parser =
    Parser.succeed Internal.fromNonemptyRules
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
            , Pratt.prefix 3 (Parser.token (Parser.Token "&" ExpectingAmpersand)) Lookahead
            , Pratt.literal <| Parser.map Literal literal
            , Pratt.literal <| Parser.map Tag tag
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 1 (Parser.token (Parser.Token "|" ExpectingPipe)) Alternation
            , Pratt.infixLeft 2 spacesOnly Concatenation
            , Pratt.postfix 4 (Parser.token (Parser.Token "+" ExpectingPlusSign)) OneOrMore
            , Pratt.postfix 5 (Parser.token (Parser.Token "*" ExpectingAsterisk)) ZeroOrMore
            , Pratt.postfix 6 (Parser.token (Parser.Token "?" ExpectingQuestionMark)) Optional
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
        , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
        , reserved = Set.empty
        }
        |> Parser.inContext InTag
