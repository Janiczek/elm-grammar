module Tests exposing (grammarParsing, usage)

import Dict
import Expect
import Grammar exposing (Config, Error(..), Parser, Structure(..))
import Grammar.Internal exposing (Strategy(..))
import Grammar.Parser exposing (Problem(..))
import Parser exposing (Problem(..))
import Test exposing (Test)


usage : Test
usage =
    let
        runOn : String -> Parser -> Result Error Structure
        runOn input parser =
            Grammar.run parser input

        runWithOn : Config -> String -> Parser -> Result Error Structure
        runWithOn config input parser =
            Grammar.runWith config parser input
    in
    Test.describe "Grammar.parser usage"
        [ Test.test "literal Ok example" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast"
                    """
                    |> Result.andThen (runOn "toast")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "toast" ]))
        , Test.test "literal Err example" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast"
                    """
                    |> Result.andThen (runOn "sausage")
                    |> Expect.equal
                        (Err
                            (ParseProblem
                                [ { col = 1
                                  , problem = ExpectingLiteral "toast"
                                  , contextStack = []
                                  , row = 1
                                  }
                                ]
                            )
                        )
        , Test.test "two literals - first OK" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast"
                    bread -> "bagel"
                    """
                    |> Result.andThen (runOn "toast")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "toast" ]))
        , Test.test "two literals - second OK" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast"
                    bread -> "bagel"
                    """
                    |> Result.andThen (runOn "bagel")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "bagel" ]))
        , Test.test "tag is used if first" <|
            \() ->
                Grammar.parser
                    """
                    breakfast -> bread
                    bread -> "toast"
                    """
                    |> Result.andThen (runOn "toast")
                    |> Expect.equal (Ok (Node "breakfast" [ Node "bread" [ Terminal "toast" ] ]))
        , Test.test "tag is not used if second" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast"
                    breakfast -> bread
                    """
                    |> Result.andThen (runOn "toast")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "toast" ]))
        , Test.test "concatenation" <|
            \() ->
                Grammar.parser
                    """
                    breakfast -> "breakfast: " bread
                    bread -> "toast"
                    """
                    |> Result.andThen (runOn "breakfast: toast")
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
                    |> Result.andThen (runOn "toast")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "toast" ]))
        , Test.test "alternation: second OK" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast" | "bagel"
                    """
                    |> Result.andThen (runOn "bagel")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "bagel" ]))
        , Test.test "hidden: doesn't show up" <|
            \() ->
                Grammar.parser
                    """
                    bread -> "toast" | <"yummy "> "bagel"
                    """
                    |> Result.andThen (runOn "yummy bagel")
                    |> Expect.equal (Ok (Node "bread" [ Terminal "bagel" ]))
        , Test.test "grouping: 'ac' is OK" <|
            \() ->
                Grammar.parser
                    """
                    s -> ("a" | "b") "c"
                    """
                    |> Result.andThen (runOn "ac")
                    |> Expect.equal
                        (Ok
                            (Node "s"
                                [ Terminal "a"
                                , Terminal "c"
                                ]
                            )
                        )
        , Test.test "grouping: 'bc' is OK" <|
            \() ->
                Grammar.parser
                    """
                    s -> ("a" | "b") "c"
                    """
                    |> Result.andThen (runOn "bc")
                    |> Expect.equal
                        (Ok
                            (Node "s"
                                [ Terminal "b"
                                , Terminal "c"
                                ]
                            )
                        )
        , Test.test "one or more: zero doesn't work" <|
            \() ->
                Grammar.parser
                    """
                    s -> "a"+
                    """
                    |> Result.andThen (runOn "")
                    |> Expect.err
        , Test.test "one or more: one works" <|
            \() ->
                Grammar.parser
                    """
                    s -> "a"+
                    """
                    |> Result.andThen (runOn "a")
                    |> Expect.equal (Ok (Node "s" [ Terminal "a" ]))
        , Test.test "one or more: two work" <|
            \() ->
                Grammar.parser
                    """
                    s -> "a"+
                    """
                    |> Result.andThen (runOn "aa")
                    |> Expect.equal
                        (Ok
                            (Node "s"
                                [ Terminal "a"
                                , Terminal "a"
                                ]
                            )
                        )
        , Test.test "zero or more: zero works" <|
            \() ->
                Grammar.parser
                    """
                    s -> "a"*
                    """
                    |> Result.andThen (runOn "")
                    |> Expect.equal (Ok (Node "s" []))
        , Test.test "zero or more: one works" <|
            \() ->
                Grammar.parser
                    """
                    s -> "a"*
                    """
                    |> Result.andThen (runOn "a")
                    |> Expect.equal (Ok (Node "s" [ Terminal "a" ]))
        , Test.test "zero or more: two work" <|
            \() ->
                Grammar.parser
                    """
                    s -> "a"*
                    """
                    |> Result.andThen (runOn "aa")
                    |> Expect.equal
                        (Ok
                            (Node "s"
                                [ Terminal "a"
                                , Terminal "a"
                                ]
                            )
                        )
        , Test.test "optional: zero works" <|
            \() ->
                Grammar.parser
                    """
                    s -> "a"?
                    """
                    |> Result.andThen (runOn "")
                    |> Expect.equal (Ok (Node "s" []))
        , Test.test "optional: one works" <|
            \() ->
                Grammar.parser
                    """
                    s -> "a"?
                    """
                    |> Result.andThen (runOn "a")
                    |> Expect.equal (Ok (Node "s" [ Terminal "a" ]))
        , Test.test "optional: two don't work with default config (partial = False)" <|
            \() ->
                Grammar.parser
                    """
                    s -> "a"?
                    """
                    |> Result.andThen (runOn "aa")
                    |> Expect.err
        , Test.test "optional: two work with partial = True" <|
            \() ->
                Grammar.parser
                    """
                    s -> "a"?
                    """
                    |> Result.andThen
                        (runWithOn
                            { partial = True
                            , start = Nothing
                            }
                            "aa"
                        )
                    |> Expect.equal (Ok (Node "s" [ Terminal "a" ]))
        , Test.test "custom start" <|
            \() ->
                Grammar.parser
                    """
                    s -> s
                    a -> "a"
                    """
                    |> Result.andThen
                        (runWithOn
                            { partial = False
                            , start = Just "a"
                            }
                            "a"
                        )
                    |> Expect.equal (Ok (Node "a" [ Terminal "a" ]))
        , Test.test "Larger example from the book Crafting Interpreters" <|
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
                    |> Result.andThen (runOn "poached eggs with toast on the side")
                    |> Expect.equal
                        (Ok
                            (Node "breakfast"
                                [ Node "protein"
                                    [ Node "cooked" [ Terminal "poached" ]
                                    , Terminal " eggs"
                                    ]
                                , Terminal " with "
                                , Node "breakfast"
                                    [ Node "bread"
                                        [ Terminal "toast" ]
                                    ]
                                , Terminal " on the side"
                                ]
                            )
                        )
        , Test.test "Cleaner output of larger example with hiding" <|
            \() ->
                Grammar.parser
                    """
breakfast  -> protein <" with "> breakfast <" on the side">
breakfast  -> protein 
breakfast  -> bread 

protein    -> crispiness " crispy bacon" 
protein    -> "sausage" 
protein    -> cooked <" "> "eggs" 

crispiness -> "really" 
crispiness -> "really" <" "> crispiness 

cooked     -> "scrambled" 
cooked     -> "poached" 
cooked     -> "fried" 

bread      -> "toast" 
bread      -> "biscuits" 
bread      -> "English muffin" 
                    """
                    |> Result.andThen (runOn "poached eggs with toast on the side")
                    |> Expect.equal
                        (Ok
                            (Node "breakfast"
                                [ Node "protein"
                                    [ Node "cooked" [ Terminal "poached" ]
                                    , Terminal "eggs"
                                    ]
                                , Node "breakfast"
                                    [ Node "bread"
                                        [ Terminal "toast" ]
                                    ]
                                ]
                            )
                        )
        ]


grammarParsing : Test
grammarParsing =
    Test.describe "Grammar.Parser.parse"
        [ Test.test "simple terminal" <|
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
        , Test.test "hidden" <|
            \() ->
                Grammar.Parser.parse
                    """
                    bread -> <"crispy"> "toast"
                    """
                    |> Expect.equal
                        (Ok
                            { start = "bread"
                            , rules =
                                Dict.fromList
                                    [ ( "bread"
                                      , ( Concatenation
                                            (Hidden (Literal "crispy"))
                                            (Literal "toast")
                                        , []
                                        )
                                      )
                                    ]
                            }
                        )
        , Test.test "grouped" <|
            \() ->
                Grammar.Parser.parse
                    """
                    s -> ("a" | "b") "c"
                    """
                    |> Expect.equal
                        (Ok
                            { start = "s"
                            , rules =
                                Dict.fromList
                                    [ ( "s"
                                      , ( Concatenation
                                            (Alternation
                                                (Literal "a")
                                                (Literal "b")
                                            )
                                            (Literal "c")
                                        , []
                                        )
                                      )
                                    ]
                            }
                        )
        , Test.test "one or more" <|
            \() ->
                Grammar.Parser.parse
                    """
                    s -> "a"+
                    """
                    |> Expect.equal
                        (Ok
                            { start = "s"
                            , rules = Dict.fromList [ ( "s", ( OneOrMore (Literal "a"), [] ) ) ]
                            }
                        )
        , Test.test "zero or more" <|
            \() ->
                Grammar.Parser.parse
                    """
                    s -> "a"*
                    """
                    |> Expect.equal
                        (Ok
                            { start = "s"
                            , rules = Dict.fromList [ ( "s", ( ZeroOrMore (Literal "a"), [] ) ) ]
                            }
                        )
        , Test.test "optional" <|
            \() ->
                Grammar.Parser.parse
                    """
                    s -> "a"?
                    """
                    |> Expect.equal
                        (Ok
                            { start = "s"
                            , rules = Dict.fromList [ ( "s", ( Optional (Literal "a"), [] ) ) ]
                            }
                        )
        ]
