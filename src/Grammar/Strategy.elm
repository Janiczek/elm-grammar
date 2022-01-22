module Grammar.Strategy exposing (Strategy(..))

{-| Strategy is the internal representation of the parsed context-free grammar.

You can also combine these programatically and then compile them into runnable parsers with [`Grammar.fromList`](Grammar#fromList).

Here are all the combinators:

@docs Strategy

-}

import Regex exposing (Regex)


{-| And here's how they correspond to the syntax from `Grammar.fromString`:

     - Literal:       "a"
     - Regex:         /\d+/
     - Tag:           a
     - Concatenation: a b
     - Alternation:   a | b
     - Hidden:        <a>
     - Optional:      a?
     - ZeroOrMore:    a*
     - OneOrMore:     a+
     - Lookahead:     &a
     - EOF:           EOF

Parts of the syntax that don't show up as a Strategy constructor:

     - Grouping:      (a)
     - Comment:       (* hello *) "world"
     - Hiding tags:   <s> -> "abc"

-}
type Strategy
    = Literal String
    | Regex Regex
    | Tag String
    | Concatenation Strategy Strategy
    | Alternation Strategy Strategy
    | Hidden Strategy
    | Optional Strategy
    | ZeroOrMore Strategy
    | OneOrMore Strategy
    | Lookahead Strategy
    | EOF
