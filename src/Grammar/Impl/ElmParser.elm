module Grammar.Impl.ElmParser exposing (runGrammar)

import Dict exposing (Dict)
import Grammar.Config as Config exposing (Config)
import Grammar.Error exposing (Error(..))
import Grammar.Internal as Internal exposing (Grammar)
import Grammar.Parser exposing (Context, Problem(..))
import Grammar.Strategy exposing (Strategy(..))
import Grammar.Structure exposing (Structure(..))
import List.ExtraExtra as List
import NonemptyList exposing (NonemptyList)
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Extra as Parser


runGrammar : Grammar -> Config -> String -> Result Error Structure
runGrammar grammar config input =
    {- This is the moment we decide to go the recursive descent path:
       converting the grammar to an `elm/parser` parser.

       This might be a decision that locks us out of some functionality: in
       the [Instaparse talk](https://www.youtube.com/watch?v=b2AUW6psVcE),
       Mark Engelberg says that for left recursion he needed the GLL parsing
       algorithm and async/dataflow approach.
    -}
    Parser.run
        (toElmParser config grammar)
        input
        |> Result.mapError ElmParserError


toElmParser : Config -> Grammar -> Grammar.Parser.Parser Structure
toElmParser config grammar =
    let
        realStart =
            Maybe.withDefault grammar.start config.start

        parserFromGrammar =
            case Dict.get realStart grammar.rules of
                Nothing ->
                    Parser.problem (CouldntFindStartingRuleOnLeftSide realStart)

                Just strategy ->
                    strategy
                        |> strategyParser grammar.rules
                        |> Parser.map (Node realStart)
    in
    if config.partial then
        parserFromGrammar

    else
        Parser.succeed identity
            |= parserFromGrammar
            |. Parser.end ExpectingEndOfString


strategyParser : Dict String Strategy -> Strategy -> Grammar.Parser.Parser (List Structure)
strategyParser rules strategy =
    let
        strategyParser_ s =
            Parser.lazy (\() -> strategyParser rules s)
    in
    case strategy of
        Concatenation s1 s2 ->
            Parser.succeed (++)
                |= strategyParser_ s1
                |= strategyParser_ s2

        Alternation s1 s2 ->
            [ s1, s2 ]
                |> List.map (Parser.backtrackable << strategyParser_)
                |> Parser.oneOf

        Literal literal ->
            Parser.succeed [ Terminal literal ]
                |. Parser.token (Parser.Token literal (ExpectingLiteral literal))

        Tag tag ->
            case Dict.get tag rules of
                Nothing ->
                    Parser.problem (CouldntFindRuleOnLeftSide tag)

                Just childStrategy ->
                    childStrategy
                        |> strategyParser_
                        |> Parser.map (\strategy_ -> [ Node tag strategy_ ])

        Hidden s ->
            Parser.succeed []
                |. strategyParser_ s

        OneOrMore s ->
            Parser.succeed (NonemptyList.toList >> List.fastConcat)
                |= Parser.some (strategyParser_ s)

        ZeroOrMore s ->
            Parser.succeed List.fastConcat
                |= Parser.many (strategyParser_ s)

        Optional s ->
            Parser.oneOf
                [ strategyParser_ s
                , Parser.succeed []
                ]

        Lookahead s ->
            Parser.succeed []
                |. (Parser.succeed String.dropLeft
                        |= Parser.getOffset
                        |= Parser.getSource
                        |> Parser.andThen
                            (\restOfInput ->
                                case Parser.run (strategyParser_ s) restOfInput of
                                    Err _ ->
                                        Parser.problem ExpectingLookahead

                                    Ok _ ->
                                        Parser.succeed ()
                            )
                   )
