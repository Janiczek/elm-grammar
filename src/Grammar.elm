module Grammar exposing
    ( Parser, fromString, fromList, fromNonemptyList, never
    , Structure(..), run, runWith
    , Config, defaultConfig
    , Error(..)
    )

{-|

@docs Parser, fromString, fromList, fromNonemptyList, never

@docs Structure, run, runWith

@docs Config, defaultConfig

@docs Error

-}

import Dict exposing (Dict)
import Grammar.Internal as Internal exposing (Grammar)
import Grammar.Parser exposing (Context, Problem(..))
import Grammar.Strategy exposing (Strategy(..))
import List.ExtraExtra as List
import NonemptyList exposing (NonemptyList)
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Extra as Parser



-- TODO visualize (toDOT)
-- TODO generate values
-- TODO toString
-- TODO expose Grammar.Parser.parse somehow?


{-| The parsed tree `Structure`.

In case you want to hide some values, use the angle brackets in your grammar: `<a>`

-}
type Structure
    = Terminal String
    | Node String (List Structure)


{-| All the ways the grammar construction or parsing can go wrong.
-}
type Error
    = -- TODO get rid of the DeadEnds and use our custom errors
      GrammarProblem (List (Parser.DeadEnd Context Problem))
    | ParseProblem (List (Parser.DeadEnd Context Problem))
    | RuleListWasEmpty
    | NeverParserUsed


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


{-| A way to customize how to run the parser.

  - `partial` means the parser will not fail if finishing before the end of the input string.
  - `start` lets you customize the starting point: by default it is the first rule in the grammar string.

-}
type alias Config =
    { partial : Bool
    , start : Maybe String
    }


{-| By default, we require the parser to consume the whole input, and we start with the first rule in the grammar.
-}
defaultConfig : Config
defaultConfig =
    { partial = False
    , start = Nothing
    }


{-| A way to actually run a compiled parser.

Uses the default config.

-}
run : Parser -> String -> Result Error Structure
run parser_ input =
    runWith defaultConfig parser_ input


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
        |> Result.mapError GrammarProblem
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
                |> List.map strategyParser_
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
                        |> List.map strategyParser_
                        |> Parser.oneOf
                        |> Parser.map (\strategies_ -> [ Node tag strategies_ ])

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
