module Grammar.Internal exposing (Grammar, Strategy(..))

import Dict exposing (Dict)
import NonemptyList exposing (NonemptyList)


type alias Grammar =
    { start : String
    , rules : Dict String (NonemptyList Strategy)
    }


{-| Implemented:

     - Literal:       s -> "a"
     - Tag:           s -> a
     - Concatenation: s -> a b
     - Alternation:   s -> a | b
     - Grouping:      s -> (a)
     - Hiding:        s -> <a>
     - Optional:      s -> a?
     - Zero or more:  s -> a*
     - One or more:   s -> a+
     - Lookahead:     s -> &a

TODO:

     - Regex:         s  -> /"[^"]*"/
     - Comment:       s  -> (* hello *) "world"
     - Hiding tags:  <s> -> "abc"

-}
type Strategy
    = Concatenation Strategy Strategy
    | Alternation Strategy Strategy
    | Literal String
    | Tag String
    | Hidden Strategy
    | OneOrMore Strategy
    | ZeroOrMore Strategy
    | Optional Strategy
    | Lookahead Strategy
