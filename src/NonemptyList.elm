module NonemptyList exposing
    ( NonemptyList
    , foldl
    , fromCons
    , fromList
    , head
    , toList
    )


type alias NonemptyList a =
    ( a, List a )


fromCons : a -> List a -> NonemptyList a
fromCons x xs =
    ( x, xs )


fromList : List a -> Maybe (NonemptyList a)
fromList list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, xs )


toList : NonemptyList a -> List a
toList ( x, xs ) =
    x :: xs


foldl : (a -> b -> b) -> b -> NonemptyList a -> b
foldl fn init ( x, xs ) =
    List.foldl fn (fn x init) xs


head : NonemptyList a -> a
head ( x, _ ) =
    x
