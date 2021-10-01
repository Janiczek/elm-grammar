module Tests exposing (grammarParsing, usage)

import Expect
import Grammar exposing (Error(..), Structure(..))
import Grammar.Internal exposing (Chunk(..))
import Grammar.Parser
import Parser exposing (Problem(..))
import Test exposing (Test)


usage : Test
usage =
    Test.describe "Grammar.parser usage"
        [ Test.test "literal Ok example" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast"
                    """
                    |> Result.andThen (Grammar.runOn "toast")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "toast" ]))
        , Test.test "literal Err example" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast"
                    """
                    |> Result.andThen (Grammar.runOn "sausage")
                    |> Expect.equal (Err (ParseProblem []))
        , Test.test "two literals - first OK" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast"
                    bread -> "bagel"
                    """
                    |> Result.andThen (Grammar.runOn "toast")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "toast" ]))
        , Test.test "two literals - second OK" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast"
                    bread -> "bagel"
                    """
                    |> Result.andThen (Grammar.runOn "bagel")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "bagel" ]))
        , Test.test "tag is used if first" <|
            \() ->
                Grammar.parser
                    """
                    breakfast -> bread
                    bread -> "toast"
                    """
                    |> Result.andThen (Grammar.runOn "toast")
                    |> Expect.equal (Ok (Node "breakfast" [ Node "bread" [ Terminal "toast" ] ]))
        , Test.test "tag is not used if second" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast"
                    breakfast -> bread
                    """
                    |> Result.andThen (Grammar.runOn "toast")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "toast" ]))
        , Test.test "multiple items in sequence" <|
            \() ->
                Grammar.parser
                    """
                    breakfast -> "breakfast: " bread
                    bread -> "toast"
                    """
                    |> Result.andThen (Grammar.runOn "breakfast: toast")
                    |> Expect.equal
                        (Ok
                            (Node "breakfast"
                                [ Terminal "breakfast: "
                                , Node "bread" [ Terminal "toast" ]
                                ]
                            )
                        )
        ]


grammarParsing : Test
grammarParsing =
    Test.describe "Grammar.Parser.parse"
        [ Test.test "nonsensical" <|
            \() ->
                Grammar.Parser.parse
                    """
                    blabla hello
                    """
                    |> Expect.equal (Err [ { col = 28, problem = ExpectingSymbol "->", row = 2 } ])
        , Test.test "simple terminal" <|
            \() ->
                Grammar.Parser.parse
                    """
                    bread -> "toast"
                    """
                    |> Expect.equal
                        (Ok
                            [ { tag = "bread"
                              , sequence = [ Literal "toast" ]
                              }
                            ]
                        )
        , Test.test "two terminals for same tag" <|
            \() ->
                Grammar.Parser.parse
                    """
                    bread -> "toast"
                    bread -> "bagel"
                    """
                    |> Expect.equal
                        (Ok
                            [ { tag = "bread"
                              , sequence = [ Literal "toast" ]
                              }
                            , { tag = "bread"
                              , sequence = [ Literal "bagel" ]
                              }
                            ]
                        )
        , Test.test "tag and terminal" <|
            \() ->
                Grammar.Parser.parse
                    """
                    bread -> "toast"
                    breakfast -> bread
                    """
                    |> Expect.equal
                        (Ok
                            [ { tag = "bread"
                              , sequence = [ Literal "toast" ]
                              }
                            , { tag = "breakfast"
                              , sequence = [ Tag "bread" ]
                              }
                            ]
                        )
        , Test.test "multiple items in sequence" <|
            \() ->
                Grammar.Parser.parse
                    """
                    bread -> "toast"
                    breakfast -> "breakfast" bread
                    """
                    |> Expect.equal
                        (Ok
                            [ { tag = "bread"
                              , sequence = [ Literal "toast" ]
                              }
                            , { tag = "breakfast"
                              , sequence =
                                    [ Literal "breakfast"
                                    , Tag "bread"
                                    ]
                              }
                            ]
                        )
        ]
