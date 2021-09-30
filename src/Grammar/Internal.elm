module Grammar.Internal exposing (Chunk(..), Grammar, Rule)


type alias Grammar =
    List Rule


type alias Rule =
    { tag : String
    , sequence : List Chunk
    }


type Chunk
    = Literal String
    | Tag String
