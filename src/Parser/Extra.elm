module Parser.Extra exposing (advanceChars, fromMaybe, many, some)

import NonemptyList exposing (NonemptyList)
import Parser.Advanced as Parser exposing ((|.), (|=), Parser, Step(..))


fromMaybe : x -> Maybe a -> Parser c x a
fromMaybe problem maybeVal =
    case maybeVal of
        Nothing ->
            Parser.problem problem

        Just val ->
            Parser.succeed val



-- Copied from Punie/elm-parser-extras:


many : Parser c x a -> Parser c x (List a)
many p =
    Parser.loop [] (manyHelp p)


some : Parser c x a -> Parser c x (NonemptyList a)
some p =
    Parser.succeed NonemptyList.fromCons
        |= p
        |= many p


manyHelp : Parser c x a -> List a -> Parser c x (Step (List a) (List a))
manyHelp p vs =
    Parser.oneOf
        [ Parser.succeed (\v -> Loop (v :: vs))
            |= p
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse vs))
        ]


advanceChars : Int -> x -> Parser c x a -> Parser c x a
advanceChars n impossibleError parser =
    let
        go : Int -> Parser c x a -> Parser c x a
        go todo accParser =
            if todo <= 0 then
                accParser

            else
                go
                    (todo - 1)
                    (accParser |. Parser.chompIf (\_ -> True) impossibleError)
    in
    go n parser
