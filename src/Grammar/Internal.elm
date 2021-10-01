module Grammar.Internal exposing (Grammar, Strategy(..))

import Dict exposing (Dict)
import NonemptyList exposing (NonemptyList)


type alias Grammar =
    { start : String
    , rules : Dict String (NonemptyList Strategy)
    }


{-| TODO:

     - Optional:      s  -> a?
     - Zero or more:  s  -> a*
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
