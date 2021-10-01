module Parser.Extra exposing (fromMaybe)

import Parser exposing (Parser)


fromMaybe : String -> Maybe a -> Parser a
fromMaybe problem maybeVal =
    case maybeVal of
        Nothing ->
            Parser.problem problem

        Just val ->
            Parser.succeed val
