module Grammar exposing (Error(..), Parser, Structure(..), parser)

import Grammar.Internal exposing (Chunk(..), Grammar, Rule)
import Grammar.Parser
import Parser


type Structure
    = Terminal String
    | Node String (List Structure)


{-| TODO get rid of the DeadEnds and use our custom errors
-}
type Error
    = GrammarProblem (List Parser.DeadEnd)
    | ParseProblem (List Parser.DeadEnd)


type alias Parser =
    String -> Result Error Structure


parser : String -> Result Error Parser
parser grammarString =
    Grammar.Parser.parse grammarString
        |> Result.mapError GrammarProblem
        |> Result.map runGrammar


runGrammar : Grammar -> String -> Result Error Structure
runGrammar grammar input =
    Parser.run (parserFromGrammar grammar) input
        |> Result.mapError ParseProblem


parserFromGrammar : Grammar -> Parser.Parser Structure
parserFromGrammar grammar =
    Debug.todo "Grammar.parserFromGrammar"
