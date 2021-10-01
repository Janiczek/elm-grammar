module Grammar exposing (Config, Error(..), Parser, Structure(..), defaultConfig, never, parser, run, runWith)

import Dict exposing (Dict)
import Grammar.Internal exposing (Grammar, Strategy(..))
import Grammar.Parser exposing (Context, Problem(..))
import List.ExtraExtra as List
import NonemptyList exposing (NonemptyList)
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Extra as Parser



-- TODO visualize (toDOT)
-- TODO generate values


type Structure
    = Terminal String
    | Node String (List Structure)


{-| TODO get rid of the DeadEnds and use our custom errors
-}
type Error
    = GrammarProblem (List (Parser.DeadEnd Context Problem))
    | ParseProblem (List (Parser.DeadEnd Context Problem))
    | NeverParserUsed


type Parser
    = Parser (Config -> String -> Result Error Structure)


type alias Config =
    { partial : Bool
    , start : Maybe String
    }


defaultConfig : Config
defaultConfig =
    { partial = False
    , start = Nothing
    }


run : Parser -> String -> Result Error Structure
run parser_ input =
    runWith defaultConfig parser_ input


runWith : Config -> Parser -> String -> Result Error Structure
runWith config (Parser parser_) input =
    parser_ config input


never : Parser
never =
    Parser (\_ _ -> Err NeverParserUsed)


parser : String -> Result Error Parser
parser grammarString =
    Grammar.Parser.parse grammarString
        |> Result.mapError GrammarProblem
        |> Result.map (\grammar -> Parser (runGrammar grammar))


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
        |> Result.mapError ParseProblem


toElmParser : Config -> Grammar -> Grammar.Parser.Parser Structure
toElmParser config grammar =
    let
        realStart =
            Maybe.withDefault grammar.start config.start

        parserFromGrammar =
            case Dict.get realStart grammar.rules of
                Nothing ->
                    Parser.problem (CouldntFindStartingRuleOnLeftSide realStart)

                Just strategies ->
                    strategies
                        |> NonemptyList.toList
                        |> List.map (strategyParser grammar.rules)
                        |> Parser.oneOf
                        |> Parser.map (Node realStart)
    in
    if config.partial then
        parserFromGrammar

    else
        Parser.succeed identity
            |= parserFromGrammar
            |. Parser.end ExpectingEndOfString


strategyParser : Dict String (NonemptyList Strategy) -> Strategy -> Grammar.Parser.Parser (List Structure)
strategyParser rules strategy =
    case strategy of
        Concatenation s1 s2 ->
            Parser.succeed (++)
                |= Parser.lazy (\() -> strategyParser rules s1)
                |= Parser.lazy (\() -> strategyParser rules s2)

        Alternation s1 s2 ->
            [ s1, s2 ]
                |> List.map (\s -> Parser.lazy (\() -> strategyParser rules s))
                |> Parser.oneOf

        Literal literal ->
            Parser.succeed [ Terminal literal ]
                |. Parser.token (Parser.Token literal (ExpectingLiteral literal))

        Tag tag ->
            case Dict.get tag rules of
                Nothing ->
                    Parser.problem (CouldntFindRuleOnLeftSide tag)

                Just strategies ->
                    strategies
                        |> NonemptyList.toList
                        |> List.map (\s -> Parser.lazy (\() -> strategyParser rules s))
                        |> Parser.oneOf
                        |> Parser.map (\strategies_ -> [ Node tag strategies_ ])

        Hidden s ->
            Parser.succeed []
                |. Parser.lazy (\() -> strategyParser rules s)

        OneOrMore s ->
            Parser.succeed (NonemptyList.toList >> List.fastConcat)
                |= Parser.some (Parser.lazy (\() -> strategyParser rules s))

        ZeroOrMore s ->
            Parser.succeed List.fastConcat
                |= Parser.many (Parser.lazy (\() -> strategyParser rules s))

        Optional s ->
            Parser.oneOf
                [ Parser.lazy (\() -> strategyParser rules s)
                , Parser.succeed []
                ]
