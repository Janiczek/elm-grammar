module Tests exposing (grammarParsing, usage)

import Dict
import Expect
import Grammar exposing (Error(..), Structure(..))
import Grammar.Internal exposing (Strategy(..))
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
                    |> Expect.equal (Err (ParseProblem [ { col = 1, problem = Expecting "toast", row = 1 } ]))
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
        , Test.test "concatenation" <|
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
        , Test.test "alternation: first OK" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast" | "bagel"
                    """
                    |> Result.andThen (Grammar.runOn "toast")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "toast" ]))
        , Test.test "alternation: second OK" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast" | "bagel"
                    """
                    |> Result.andThen (Grammar.runOn "bagel")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "bagel" ]))
        , Test.skip <|
            Test.test "Larger example from the book Crafting Interpreters" <|
                \() ->
                    Grammar.parser
                        """
breakfast  -> protein " with " breakfast " on the side" 
breakfast  -> protein 
breakfast  -> bread 

protein    -> crispiness " crispy bacon" 
protein    -> "sausage" 
protein    -> cooked " eggs" 

crispiness -> "really" 
crispiness -> "really " crispiness 

cooked     -> "scrambled" 
cooked     -> "poached" 
cooked     -> "fried" 

bread      -> "toast" 
bread      -> "biscuits" 
bread      -> "English muffin" 
                    """
                        |> Result.andThen (Grammar.runOn "poached eggs with toast on the side")
                        |> Expect.equal (Ok (Node "todo" []))
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
                            { start = "bread"
                            , rules = Dict.fromList [ ( "bread", ( Literal "toast", [] ) ) ]
                            }
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
                            { start = "bread"
                            , rules =
                                Dict.fromList
                                    [ ( "bread"
                                      , ( Literal "toast"
                                        , [ Literal "bagel" ]
                                        )
                                      )
                                    ]
                            }
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
                            { start = "bread"
                            , rules =
                                Dict.fromList
                                    [ ( "bread", ( Literal "toast", [] ) )
                                    , ( "breakfast", ( Tag "bread", [] ) )
                                    ]
                            }
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
                            { start = "bread"
                            , rules =
                                Dict.fromList
                                    [ ( "bread", ( Literal "toast", [] ) )
                                    , ( "breakfast"
                                      , ( Concatenation
                                            (Literal "breakfast")
                                            (Tag "bread")
                                        , []
                                        )
                                      )
                                    ]
                            }
                        )
        , Test.test "alternation" <|
            \() ->
                Grammar.Parser.parse
                    """
                    bread -> "toast" | "bagel"
                    """
                    |> Expect.equal
                        (Ok
                            { start = "bread"
                            , rules =
                                Dict.fromList
                                    [ ( "bread"
                                      , ( Alternation
                                            (Literal "toast")
                                            (Literal "bagel")
                                        , []
                                        )
                                      )
                                    ]
                            }
                        )
        , Test.test "concatenation and alternation are grouped correctly" <|
            \() ->
                Grammar.Parser.parse
                    """
                    bread -> "crispy" "toast" | "bagel"
                    """
                    |> Expect.equal
                        (Ok
                            { start = "bread"
                            , rules =
                                Dict.fromList
                                    [ ( "bread"
                                      , ( Alternation
                                            (Concatenation
                                                (Literal "crispy")
                                                (Literal "toast")
                                            )
                                            (Literal "bagel")
                                        , []
                                        )
                                      )
                                    ]
                            }
                        )
        ]
