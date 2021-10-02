module Grammar exposing
    ( Parser, fromString, fromList, fromNonemptyList, never
    , run, runWith
    )

{-|

@docs Parser, fromString, fromList, fromNonemptyList, never

@docs run, runWith

-}

import Dict exposing (Dict)
import Grammar.Config as Config exposing (Config)
import Grammar.Error exposing (Error(..))
import Grammar.Impl.GLL as GLL
import Grammar.Internal as Internal exposing (Grammar)
import Grammar.Parser exposing (Context, Problem)
import Grammar.Strategy exposing (Strategy(..))
import Grammar.Structure exposing (Structure(..))
import List.ExtraExtra as List
import NonemptyList exposing (NonemptyList)



-- TODO visualize (toDOT)
-- TODO generate values
-- TODO toString
-- TODO expose Grammar.Parser.parse somehow?
-- TODO automatic whitespace


{-| `Parser` is able to transform your input into a tree structure.

  - Create it with
      - [`Grammar.fromString`](Grammar#fromString)
      - [`Grammar.fromList`](Grammar#fromList)
      - [`Grammar.fromNonemptyList`](Grammar#fromNonemptyList)

  - Run it with
      - [`Grammar.run`](Grammar#run)
      - [`Grammar.runWith`](Grammar#runWith)

-}
type Parser
    = Parser (Config -> String -> Result Error Structure)


{-| A way to actually run a compiled parser.

Uses the default config.

-}
run : Parser -> String -> Result Error Structure
run parser_ input =
    runWith Config.defaultConfig parser_ input


{-| A way to actually run a compiled parser with possibly customized config.
-}
runWith : Config -> Parser -> String -> Result Error Structure
runWith config (Parser parser_) input =
    parser_ config input


{-| Similar to `elm/regex`, this is something to provide to `Maybe.withDefault` when you're compiling a parser and are sure it will succeed.

This parser will always fail when used.

Read also: <https://jfmengels.net/safe-unsafe-operations-in-elm/>

-}
never : Parser
never =
    Parser (\_ _ -> Err NeverParserUsed)


{-| A convenient way to compile a grammar in a human-readable string form to a runnable parser.

    Grammar.fromString
        """
        expr -> number | parenthesized | op-usage

        number -> digit+
        digit  -> "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

        op       -> "+" | "-" | "*" | "/"
        op-usage -> expr op expr

        parenthesized -> <"("> expr <")">
        """
        --> Ok (Parser <function>)

-}
fromString : String -> Result Error Parser
fromString grammarString =
    Grammar.Parser.parse grammarString
        |> Result.mapError GrammarError
        |> Result.map (\grammar -> Parser (runGrammar grammar))


{-| You can create your own grammar programatically instead of as a string: see the `Strategy` type. This is a way to compile it to a parser that you can then run.
-}
fromList : List ( String, Strategy ) -> Result Error Parser
fromList rules =
    rules
        |> Internal.fromRules
        |> Result.fromMaybe RuleListWasEmpty
        |> Result.map (\grammar -> Parser (runGrammar grammar))


{-| You can create your own grammar programatically instead of as a string: see the `Strategy` type. This is a way to compile it to a parser that you can then run.

A non-empty list alternative to [`Grammar.fromList`](Grammar#fromList) that guarantees the parser will successfully compile.

-}
fromNonemptyList : NonemptyList ( String, Strategy ) -> Parser
fromNonemptyList rules =
    rules
        |> Internal.fromNonemptyRules
        |> runGrammar
        |> Parser


runGrammar : Grammar -> Config -> String -> Result Error Structure
runGrammar =
    -- ElmParser.runGrammar
    GLL.runGrammar
