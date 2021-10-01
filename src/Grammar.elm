module Grammar exposing (Error(..), Parser, Structure(..), never, parser, run, runOn)

import Grammar.Internal exposing (Chunk(..), Grammar, Rule)
import Grammar.Parser
import Parser exposing ((|.), (|=))


type Structure
    = Terminal String
    | Node String (List Structure)


{-| TODO get rid of the DeadEnds and use our custom errors
-}
type Error
    = GrammarProblem (List Parser.DeadEnd)
    | ParseProblem (List Parser.DeadEnd)
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


toElmParser : Grammar -> Parser.Parser Structure
toElmParser grammar =
    Parser.oneOf
        [ Debug.todo "Grammar.toElmParser"
        ]
