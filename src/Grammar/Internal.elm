module Grammar.Internal exposing (Chunk(..), Grammar, Rule)

import NonemptyList exposing (NonemptyList)


type alias Grammar =
    List Rule


type alias Rule =
    { tag : String
    , sequence : NonemptyList Chunk
    }


type Chunk
    = Literal String
    | Tag String
