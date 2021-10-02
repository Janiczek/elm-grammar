module Grammar.Error exposing (Error(..), GLLParserError(..))

import Grammar.Parser exposing (Context, Problem(..))
import Parser.Advanced as Parser


{-| All the ways the grammar construction or parsing can go wrong.
-}
type Error
    = -- TODO get rid of the DeadEnds and use our custom errors
      -- TODO transform ElmParserErrors to common ParserError usable from GLL too
      GrammarError (List (Parser.DeadEnd Context Problem))
    | ElmParserError (List (Parser.DeadEnd Context Problem))
    | GLLParserError GLLParserError
    | RuleListWasEmpty
    | NeverParserUsed


type GLLParserError
    = TagRuleNotFound String
    | NodeNotFound Int
    | ExpectedLiteral String
    | DidntFinish
