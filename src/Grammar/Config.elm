module Grammar.Config exposing (Config, defaultConfig)

{-|

@docs Config, defaultConfig

-}


{-| A way to customize how to run the parser.

  - `partial` means the parser will not fail if finishing before the end of the input string.
  - `start` lets you customize the starting point: by default it is the first rule in the grammar string.

-}
type alias Config =
    { partial : Bool
    , start : Maybe String
    }


{-| By default, we require the parser to consume the whole input, and we start with the first rule in the grammar.
-}
defaultConfig : Config
defaultConfig =
    { partial = False
    , start = Nothing
    }
