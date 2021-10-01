module Grammar.Internal exposing (Grammar, Strategy(..))

import Dict exposing (Dict)
import NonemptyList exposing (NonemptyList)


type alias Grammar =
    { start : String
    , rules : Dict String (NonemptyList Strategy)
    }


type Strategy
    = Concatenation Strategy Strategy
    | Alternation Strategy Strategy
    | Literal String
    | Tag String
