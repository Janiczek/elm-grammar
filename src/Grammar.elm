module Grammar exposing (Error(..), Parser, Structure(..), parser)

import Grammar.Internal exposing (Chunk(..), Grammar, Rule)
import Grammar.Parser


type Structure
    = Terminal String
    | Node String (List Structure)


type Error
    = GrammarProblem String
    | PartialMatch { unmatched : { row : Int, column : Int } }


type alias Parser =
    String -> Result Error Structure


parser : String -> Result Error Parser
parser grammarString =
    case Grammar.Parser.parse grammarString of
        Err deadEnds ->
            Err (GrammarProblem (Debug.toString deadEnds))

        Ok grammar ->
            Ok (runGrammar grammar)


runGrammar : Grammar -> String -> Result Error Structure
runGrammar grammar input =
    Debug.todo "Grammar.runGrammar"
