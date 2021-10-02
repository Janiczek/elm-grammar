module Grammar.Strategy exposing
    ( Strategy(..)
    , literalValue, toComparable
    )

{-| Strategy is the internal representation of the parsed context-free grammar.

You can also combine these programatically and then compile them into runnable parsers with [`Grammar.fromList`](Grammar#fromList).

Here are all the combinators:

@docs Strategy

-}


{-| And here's how they correspond to the syntax from `Grammar.fromString`:

     - Literal:       "a"
     - Tag:           a
     - Concatenation: a b
     - Alternation:   a | b
     - Grouping:      (a)    (not an actual constructor)
     - Hidden:        <a>
     - Optional:      a?
     - ZeroOrMore:    a*
     - OneOrMore:     a+
     - Lookahead:     &a

-}
type Strategy
    = {- TODO:
         - Regex:         s  -> /"[^"]*"/
         - Comment:       s  -> (* hello *) "world"
         - Hiding tags:  <s> -> "abc"
      -}
      Literal String
    | Tag String
    | Concatenation Strategy Strategy
    | Alternation Strategy Strategy
    | Hidden Strategy
    | Optional Strategy
    | ZeroOrMore Strategy
    | OneOrMore Strategy
    | Lookahead Strategy


toComparable : Strategy -> String
toComparable strategy =
    case strategy of
        Literal str ->
            "\"" ++ str ++ "\""

        Tag tag ->
            tag

        Concatenation s1 s2 ->
            toComparable s1
                ++ " "
                ++ toComparable s2

        Alternation s1 s2 ->
            toComparable s1
                ++ " | "
                ++ toComparable s2

        Hidden s ->
            "<" ++ toComparable s ++ ">"

        Optional s ->
            toComparable s ++ "?"

        ZeroOrMore s ->
            toComparable s ++ "*"

        OneOrMore s ->
            toComparable s ++ "+"

        Lookahead s ->
            "&" ++ toComparable s


literalValue : Strategy -> Maybe String
literalValue strategy =
    case strategy of
        Literal literal ->
            Just literal

        _ ->
            Nothing
