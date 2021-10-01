module Parser.Extra exposing (fromMaybe)

import Parser.Advanced as Parser exposing (Parser)


fromMaybe : x -> Maybe a -> Parser c x a
fromMaybe problem maybeVal =
    case maybeVal of
        Nothing ->
            Parser.problem problem

        Just val ->
            Parser.succeed val
