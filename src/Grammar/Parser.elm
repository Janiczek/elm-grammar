module Grammar.Parser exposing (Context(..), Parser, Problem(..), parse)

import Grammar.Internal as Internal exposing (Grammar, RuleVisibility(..))
import Grammar.Strategy exposing (Strategy(..))
import NonemptyList
import Parser.Advanced as Parser exposing ((|.), (|=), DeadEnd)
import Parser.Extra as Parser
import Pratt.Advanced as Pratt
import Regex exposing (Regex)
import Set


type alias Parser a =
    Parser.Parser Context Problem a


type Context
    = InLiteral
    | InHidden
    | InTagAndRuleVisibility
    | InGrouped
    | InTag
    | InStrategy
    | InRule
    | InRegex


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
    | ExpectingSolidus
    | ExpectingArrow
    | ExpectingColonColonEquals
    | ExpectingEquals
    | ExpectingNewline
    | ExpectingLiteral String
    | ExpectingPlusSign
    | ExpectingAsterisk
    | ExpectingQuestionMark
    | ExpectingEndOfString
    | ExpectingAmpersand
    | ExpectingLookahead
    | ExpectingOpeningCommentBrace
    | ExpectingClosingCommentBrace
    | ExpectingNonemptySpaces
    | ExpectingRegexMatch
    | ExpectingValidRegex
    | ExpectingOpeningRegexSlash
    | ExpectingClosingRegexSlash
    | ExpectingSemicolon
    | ExpectingDoubleDash
    | ExpectingSlash
    | ExpectingBackslash
    | ExpectingEOF


parse : String -> Result (List (DeadEnd Context Problem)) Grammar
parse string =
    Parser.run parser string


parser : Parser Grammar
parser =
    Parser.succeed Internal.fromNonemptyRules
        |. whitespaceOrMultiComment { allowNewlines = True }
        |= (Parser.sequence
                { start = empty
                , separator = Parser.Token "\n" ExpectingNewline
                , end = empty
                , spaces = whitespaceOrMultiComment { allowNewlines = False }
                , item = rule
                , trailing = Parser.Optional
                }
                |> Parser.andThen (NonemptyList.fromList >> Parser.fromMaybe RuleListWasEmpty)
           )
        |. whitespaceOrMultiComment { allowNewlines = True }


empty : Parser.Token Problem
empty =
    Parser.Token "" ShouldntHappen


whitespaceOrMultiComment : { allowNewlines : Bool } -> Parser ()
whitespaceOrMultiComment { allowNewlines } =
    let
        chompFn : Char -> Bool
        chompFn =
            if allowNewlines then
                \c -> c == ' ' || c == '\t' || c == ',' || c == '\n'

            else
                \c -> c == ' ' || c == '\t' || c == ','
    in
    Parser.loop () <|
        \() ->
            Parser.oneOf
                [ Parser.oneOf
                    [ Parser.chompWhile chompFn
                        |> Parser.getChompedString
                        |> Parser.andThen
                            (\str ->
                                if String.isEmpty str then
                                    Parser.problem ExpectingNonemptySpaces

                                else
                                    Parser.succeed ()
                            )
                    , comment
                    ]
                    |> Parser.map Parser.Loop
                , Parser.succeed (Parser.Done ())
                ]


comment : Parser ()
comment =
    let
        openingCommentBrace =
            Parser.Token "(*" ExpectingOpeningCommentBrace

        closingCommentBrace =
            Parser.Token "*)" ExpectingClosingCommentBrace
    in
    Parser.succeed ()
        |. Parser.multiComment
            openingCommentBrace
            closingCommentBrace
            Parser.NotNestable
        |. Parser.token closingCommentBrace


rule : Parser ( String, ( RuleVisibility, Strategy ) )
rule =
    Parser.succeed
        (\( tag_, ruleVisibility ) strategy_ ->
            ( tag_, ( ruleVisibility, strategy_ ) )
        )
        |. whitespaceOrMultiComment { allowNewlines = True }
        |= tagAndRuleVisibility
        |. whitespaceOrMultiComment { allowNewlines = False }
        |. arrowLike
        |. whitespaceOrMultiComment { allowNewlines = False }
        |= strategy
        |. lineComment
        |> Parser.inContext InRule


lineComment : Parser ()
lineComment =
    Parser.oneOf
        [ Parser.succeed ()
            |. Parser.oneOf
                [ Parser.token (Parser.Token ";" ExpectingSemicolon)
                , Parser.token (Parser.Token "--" ExpectingDoubleDash)
                ]
            |. Parser.chompWhile (\c -> c /= '\n')
        , Parser.succeed ()
        ]


arrowLike : Parser ()
arrowLike =
    Parser.oneOf
        [ Parser.token (Parser.Token "->" ExpectingArrow)
        , Parser.token (Parser.Token "::=" ExpectingColonColonEquals)
        , Parser.token (Parser.Token "=" ExpectingEquals)
        ]


strategy : Parser Strategy
strategy =
    Pratt.expression
        { oneOf =
            [ hidden
            , grouped
            , Pratt.prefix 3 (Parser.token (Parser.Token "&" ExpectingAmpersand)) Lookahead
            , Pratt.literal <| Parser.map (\_ -> EOF) (Parser.token (Parser.Token "EOF" ExpectingEOF))
            , Pratt.literal <| Parser.map Literal literal
            , Pratt.literal <| Parser.map Tag tag
            , Pratt.literal <| Parser.map Regex regex
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 1 alternationToken Alternation
            , Pratt.infixLeft 2 (whitespaceOrMultiComment { allowNewlines = False }) Concatenation
            , Pratt.postfix 4 (Parser.token (Parser.Token "+" ExpectingPlusSign)) OneOrMore
            , Pratt.postfix 5 (Parser.token (Parser.Token "*" ExpectingAsterisk)) ZeroOrMore
            , Pratt.postfix 6 (Parser.token (Parser.Token "?" ExpectingQuestionMark)) Optional
            ]
        , spaces = whitespaceOrMultiComment { allowNewlines = False }
        }
        |> Parser.inContext InStrategy


alternationToken : Parser ()
alternationToken =
    Parser.oneOf
        [ Parser.token (Parser.Token "|" ExpectingPipe)
        , Parser.token (Parser.Token "/" ExpectingSolidus)
        ]


hidden : Pratt.Config Context Problem Strategy -> Parser Strategy
hidden config =
    Parser.succeed Hidden
        |. Parser.token (Parser.Token "<" ExpectingLeftAngleBracket)
        |. whitespaceOrMultiComment { allowNewlines = False }
        |= Pratt.subExpression 0 config
        |. whitespaceOrMultiComment { allowNewlines = False }
        |. Parser.token (Parser.Token ">" ExpectingRightAngleBracket)
        |> Parser.inContext InHidden


grouped : Pratt.Config Context Problem Strategy -> Parser Strategy
grouped config =
    Parser.succeed identity
        |. Parser.token (Parser.Token "(" ExpectingLeftParenthesis)
        |. whitespaceOrMultiComment { allowNewlines = False }
        |= Pratt.subExpression 0 config
        |. whitespaceOrMultiComment { allowNewlines = False }
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


regex : Parser Regex
regex =
    Parser.succeed (\regex_ -> Regex.fromString ("^" ++ regex_))
        |. Parser.token (Parser.Token "/" ExpectingOpeningRegexSlash)
        |= Parser.loop [] regexHelp
        |> Parser.andThen (Parser.fromMaybe ExpectingValidRegex)
        |> Parser.inContext InRegex


regexHelp : List String -> Parser (Parser.Step (List String) String)
regexHelp revStrs =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.token (Parser.Token "\\" ExpectingBackslash)
            |= Parser.oneOf
                [ -- this makes sure we don't interpret the next `/` as ending the regex
                  Parser.succeed (Parser.Loop ("\\/" :: revStrs))
                    |. Parser.token (Parser.Token "/" ExpectingSlash)
                , Parser.succeed (Parser.Loop ("\\" :: revStrs))
                ]
        , Parser.token (Parser.Token "/" ExpectingClosingRegexSlash)
            |> Parser.map (\_ -> Parser.Done (String.join "" (List.reverse revStrs)))
        , Parser.chompWhile isUninterestingToRegex
            |> Parser.getChompedString
            |> Parser.andThen
                (\str ->
                    if String.isEmpty str then
                        Parser.problem ExpectingClosingRegexSlash

                    else
                        Parser.succeed str
                )
            |> Parser.map (\str -> Parser.Loop (str :: revStrs))
        ]


isUninterestingToRegex : Char -> Bool
isUninterestingToRegex char =
    char /= '/' && char /= '\\'


tagAndRuleVisibility : Parser ( String, RuleVisibility )
tagAndRuleVisibility =
    Parser.oneOf
        [ Parser.succeed (\tag_ -> ( tag_, RuleVisible ))
            |= tag
        , Parser.succeed (\tag_ -> ( tag_, RuleHidden ))
            |. Parser.token (Parser.Token "<" ExpectingLeftAngleBracket)
            |. whitespaceOrMultiComment { allowNewlines = False }
            |= tag
            |. whitespaceOrMultiComment { allowNewlines = False }
            |. Parser.token (Parser.Token ">" ExpectingRightAngleBracket)
        ]
        |> Parser.inContext InTagAndRuleVisibility


tag : Parser String
tag =
    Parser.variable
        { expecting = ExpectingTagName
        , start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
        , reserved = Set.empty
        }
        |> Parser.inContext InTag
