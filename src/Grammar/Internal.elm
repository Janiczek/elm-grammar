module Grammar.Internal exposing
    ( Grammar
    , fromNonemptyRules
    , fromRules
    )

import Dict exposing (Dict)
import Grammar.Strategy exposing (Strategy(..))
import List.Extra as List
import NonemptyList exposing (NonemptyList)


type alias Grammar =
    { start : String
    , rules : Dict String Strategy
    }


fromRules : List ( String, Strategy ) -> Maybe Grammar
fromRules rules =
    rules
        |> NonemptyList.fromList
        |> Maybe.map fromNonemptyRules


fromNonemptyRules : NonemptyList ( String, Strategy ) -> Grammar
fromNonemptyRules rules =
    let
        start : String
        start =
            rules
                |> NonemptyList.head
                |> Tuple.first

        groupedRules : List ( String, Strategy )
        groupedRules =
            rules
                |> NonemptyList.toList
                |> List.sortBy Tuple.first
                |> List.gatherEqualsBy Tuple.first
                |> List.map
                    (\( ( tag_, strategy1 ), grouped_ ) ->
                        let
                            restOfStrategies =
                                List.map Tuple.second grouped_
                        in
                        ( tag_, List.foldl Alternation strategy1 restOfStrategies )
                    )
    in
    { start = start
    , rules = Dict.fromList groupedRules
    }
