module List.ExtraExtra exposing (fastConcat)


fastConcat : List (List a) -> List a
fastConcat =
    List.foldr (++) []
