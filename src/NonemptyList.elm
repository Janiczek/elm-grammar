module NonemptyList exposing
    ( NonemptyList
    , foldl
    , fromList
    )


type alias NonemptyList a =
    ( a, List a )


fromList : List a -> Maybe (NonemptyList a)
fromList list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, xs )


foldl : (a -> b -> b) -> b -> NonemptyList a -> b
foldl fn init ( x, xs ) =
    List.foldl fn (fn x init) xs
