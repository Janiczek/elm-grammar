module Grammar.Strategy exposing (Strategy(..))

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
     - Grouping:      (a)                    (not an actual constructor)
     - Hidden:        <a>
     - Optional:      a?
     - ZeroOrMore:    a*
     - OneOrMore:     a+
     - Lookahead:     &a
     - Comment:       (* hello *) "world"    (not an actual constructor)

-}
type Strategy
    = {- TODO:
         - Regex:         s  -> /"[^"]*"/
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
