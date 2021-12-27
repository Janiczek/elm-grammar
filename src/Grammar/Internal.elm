module Grammar.Internal exposing
    ( Grammar
    , RuleVisibility(..)
    , fromNonemptyRules
    , fromRules
    )

import Dict exposing (Dict)
import Grammar.Strategy exposing (Strategy)
import List.Extra as List
import NonemptyList exposing (NonemptyList)


type alias Grammar =
    { start : String
    , rules : Dict String ( RuleVisibility, NonemptyList Strategy )
    }


type RuleVisibility
    = RuleVisible
    | RuleHidden


fromRules : List ( String, ( RuleVisibility, Strategy ) ) -> Maybe Grammar
fromRules rules =
    rules
        |> NonemptyList.fromList
        |> Maybe.map fromNonemptyRules


fromNonemptyRules : NonemptyList ( String, ( RuleVisibility, Strategy ) ) -> Grammar
fromNonemptyRules rules =
    let
        start : String
        start =
            rules
                |> NonemptyList.head
                |> Tuple.first

        groupedRules : List ( String, ( RuleVisibility, NonemptyList Strategy ) )
        groupedRules =
            rules
                |> NonemptyList.toList
                |> List.sortBy Tuple.first
                |> List.gatherEqualsBy Tuple.first
                |> List.map
                    (\grouped ->
                        let
                            ( tag, _ ) =
                                NonemptyList.head grouped

                            ruleVisibility =
                                if NonemptyList.any (\( _, ( rv, _ ) ) -> rv == RuleHidden) grouped then
                                    RuleHidden

                                else
                                    RuleVisible

                            strategies =
                                NonemptyList.map (\( _, ( _, strategy ) ) -> strategy) grouped
                        in
                        ( tag, ( ruleVisibility, strategies ) )
                    )
    in
    { start = start
    , rules = Dict.fromList groupedRules
    }
