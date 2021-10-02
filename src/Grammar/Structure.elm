module Grammar.Structure exposing (Structure(..))

{-| TODO rename to Tree?

@docs Structure

-}


{-| The parsed tree `Structure`.

In case you want to hide some values, use the angle brackets in your grammar: `<a>`

-}
type Structure
    = Terminal String
    | Node String Structure
    | -- TODO I think this will result in List [x, List [x, List [x, ...]]] instead of List [x,x,x,...]
      -- TODO if that happens, we should transform it (clean it up)?
      List (List Structure)
