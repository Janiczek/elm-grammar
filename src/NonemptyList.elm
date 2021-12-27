module NonemptyList exposing
    ( NonemptyList
    , any
    , foldl
    , fromCons
    , fromList
    , head
    , map
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


map : (a -> b) -> NonemptyList a -> NonemptyList b
map fn ( x, xs ) =
    ( fn x, List.map fn xs )


any : (a -> Bool) -> NonemptyList a -> Bool
any fn ( x, xs ) =
    fn x || List.any fn xs
