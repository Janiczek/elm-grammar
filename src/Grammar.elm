module Grammar exposing (Error(..), Parser, Structure(..), never, parser, run, runOn)

import Dict exposing (Dict)
import Grammar.Internal exposing (Grammar, Strategy(..))
import Grammar.Parser exposing (Context, Problem(..))
import NonemptyList exposing (NonemptyList)
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Extra as Parser


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
    = Parser (String -> Result Error Structure)


run : Parser -> String -> Result Error Structure
run (Parser parser_) input =
    parser_ input


runOn : String -> Parser -> Result Error Structure
runOn input parser_ =
    run parser_ input


never : Parser
never =
    Parser (\_ -> Err NeverParserUsed)


parser : String -> Result Error Parser
parser grammarString =
    Grammar.Parser.parse grammarString
        |> Result.mapError GrammarProblem
        |> Result.map (\grammar -> Parser (runGrammar grammar))


runGrammar : Grammar -> String -> Result Error Structure
runGrammar grammar input =
    {- This is the moment we decide to go the recursive descent path:
       converting the grammar to an `elm/parser` parser.

       This might be a decision that locks us out of some functionality: in
       the [Instaparse talk](https://www.youtube.com/watch?v=b2AUW6psVcE),
       Mark Engelberg says that for left recursion he needed the GLL parsing
       algorithm and async/dataflow approach.
    -}
    Parser.run (toElmParser grammar) input
        |> Result.mapError ParseProblem


toElmParser : Grammar -> Grammar.Parser.Parser Structure
toElmParser { start, rules } =
    case Dict.get start rules of
        Nothing ->
            Parser.problem (CouldntFindStartingRuleOnLeftSide start)

        Just strategies ->
            strategies
                |> NonemptyList.toList
                |> List.map (strategyParser rules)
                |> Parser.oneOf
                |> Parser.map (Node start)


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
