module Grammar.Impl.GLL exposing (runGrammar)

{-|

    GLL: "Generalized LL": http://ldta.info/2009/ldta2009proceedings.pdf
    Talk presenting the idea in a nice accessible way: https://www.youtube.com/watch?v=b2AUW6psVcE

-}

import Dict exposing (Dict)
import Dict.Any exposing (AnyDict)
import Dict.Extra as Dict
import Grammar.Config exposing (Config)
import Grammar.Error exposing (Error(..), GLLParserError(..))
import Grammar.Internal exposing (Grammar)
import Grammar.Strategy as Strategy exposing (Strategy(..))
import Grammar.Structure as Structure exposing (Structure(..))
import Set exposing (Set)


type alias NodeId =
    Int


type NodeType
    = TopNode
    | NormalNode
        { strategy : Strategy
        , index : Int
        }
    | CatNode
        -- Cat == Concatenation
        -- Will create a new node for the Strategy with futureListener as its listener, as soon as it gets some values from the node it's listening to
        { futureListener : NodeId
        , nextStrategy : Strategy
        }


type alias Node =
    { type_ : NodeType
    , listeners : Set NodeId
    , results : List ( Structure, Int )
    , toPrepend : Maybe Structure
    }


type alias Network =
    { nodes : Dict NodeId Node
    , reverseNodes : AnyDict String NodeType NodeId
    , nextId : NodeId
    , input : String
    , inputLength : Int
    , -- TODO rename to strategies?
      rules : Dict String Strategy
    }


runGrammar : Grammar -> Config -> String -> Result Error Structure
runGrammar grammar config input =
    {- Here we're using the dataflow GLL network instead of elm/parser. -}
    fromGrammar config input grammar
        |> Result.andThen (run input)
        |> Result.andThen
            (\n ->
                case Dict.get 0 n.nodes of
                    Nothing ->
                        Err (NodeNotFound 0)

                    Just topNode ->
                        case List.head topNode.results of
                            Nothing ->
                                Err DidntFinish

                            Just ( structure, _ ) ->
                                Ok structure
            )
        |> Result.mapError GLLParserError


nodeTypeToComparable : NodeType -> String
nodeTypeToComparable type_ =
    case type_ of
        TopNode ->
            "TopNode"

        NormalNode { strategy, index } ->
            String.join "|"
                [ "NormalNode"
                , Strategy.toComparable strategy
                , String.fromInt index
                ]

        CatNode { futureListener, nextStrategy } ->
            String.join "|"
                [ "CatNode"
                , String.fromInt futureListener
                , Strategy.toComparable nextStrategy
                ]


fromGrammar : Config -> String -> Grammar -> Result GLLParserError Network
fromGrammar config input grammar =
    let
        inputLength =
            String.length input

        -- TODO partial
        topNodeId =
            0

        topNode =
            { type_ = TopNode
            , listeners = Set.empty
            , results = []
            , toPrepend = Nothing
            }

        realStart =
            Maybe.withDefault grammar.start config.start
    in
    { nodes = Dict.singleton topNodeId topNode
    , reverseNodes = Dict.Any.singleton topNode.type_ topNodeId nodeTypeToComparable
    , nextId = topNodeId + 1
    , input = input
    , inputLength = inputLength
    , rules = grammar.rules
    }
        |> addNodeFromStrategy (Tag realStart) 0 (Just topNodeId) Nothing
        |> Result.map (\( n, _, _ ) -> n)


addNodeFromStrategy : Strategy -> Int -> Maybe NodeId -> Maybe Structure -> Network -> Result GLLParserError ( Network, NodeId, Node )
addNodeFromStrategy strategy index maybeListener toPrepend network =
    let
        node : Node
        node =
            normalNode strategy index maybeListener toPrepend
    in
    case Dict.Any.get node.type_ network.reverseNodes of
        Just existingNodeId ->
            case Dict.get existingNodeId network.nodes of
                Nothing ->
                    Err (NodeNotFound existingNodeId)

                Just existingNode ->
                    Ok ( network, existingNodeId, existingNode )

        Nothing ->
            let
                ( newNetwork, nodeId ) =
                    network
                        |> addNode node

                withChild childStrategy =
                    newNetwork
                        |> addNodeFromStrategy childStrategy index (Just nodeId) Nothing
                        |> Result.map (\( n, _, _ ) -> ( n, nodeId, node ))
            in
            case strategy of
                Literal _ ->
                    Ok ( newNetwork, nodeId, node )

                Tag tag ->
                    case Dict.get tag network.rules of
                        Nothing ->
                            Err (TagRuleNotFound tag)

                        Just childStrategy ->
                            withChild childStrategy

                Concatenation s1 s2 ->
                    newNetwork
                        |> addNodeFromStrategy s1 index Nothing Nothing
                        |> Result.andThen
                            (\( networkWithFirst, firstId, _ ) ->
                                let
                                    catNode_ =
                                        catNode s2 nodeId

                                    ( networkWithBoth, catNodeId ) =
                                        networkWithFirst
                                            |> addNode catNode_
                                in
                                networkWithBoth
                                    |> addListener { from = firstId, to = catNodeId }
                                    |> Result.map (\n -> ( n, nodeId, node ))
                            )

                Alternation s1 s2 ->
                    newNetwork
                        |> addNodeFromStrategy s1 index (Just nodeId) Nothing
                        |> Result.andThen (\( n, _, _ ) -> addNodeFromStrategy s2 index (Just nodeId) Nothing n)
                        |> Result.map (\( n, _, _ ) -> ( n, nodeId, node ))

                Hidden childStrategy ->
                    withChild childStrategy

                Optional childStrategy ->
                    withChild childStrategy

                ZeroOrMore childStrategy ->
                    withChild childStrategy

                OneOrMore childStrategy ->
                    withChild childStrategy

                Lookahead childStrategy ->
                    withChild childStrategy


normalNode : Strategy -> Int -> Maybe NodeId -> Maybe Structure -> Node
normalNode strategy index maybeListener toPrepend =
    { type_ =
        NormalNode
            { strategy = strategy
            , index = index
            }
    , listeners =
        maybeListener
            |> Maybe.map Set.singleton
            |> Maybe.withDefault Set.empty
    , results = []
    , toPrepend = toPrepend
    }


catNode : Strategy -> NodeId -> Node
catNode strategy futureListener =
    { type_ =
        CatNode
            { futureListener = futureListener
            , nextStrategy = strategy
            }
    , listeners = Set.empty
    , results = []
    , toPrepend = Nothing
    }


addNode : Node -> Network -> ( Network, NodeId )
addNode node network =
    let
        nodeId : NodeId
        nodeId =
            network.nextId

        newNetwork : Network
        newNetwork =
            { network
                | nodes = Dict.insert nodeId node network.nodes
                , reverseNodes = Dict.Any.insert node.type_ nodeId network.reverseNodes
                , nextId = network.nextId + 1
            }
    in
    ( newNetwork, nodeId )


addListener : { from : NodeId, to : NodeId } -> Network -> Result GLLParserError Network
addListener { from, to } network =
    Dict.get from network.nodes
        |> Maybe.map
            (\from_ ->
                Dict.get to network.nodes
                    |> Maybe.map
                        (\to_ ->
                            if Set.member to from_.listeners then
                                Ok network

                            else
                                let
                                    newFrom =
                                        { from_ | listeners = Set.insert to from_.listeners }

                                    newNetwork =
                                        { network | nodes = Dict.insert from newFrom network.nodes }
                                in
                                List.foldl
                                    (\result network_ -> Result.andThen (notify to (transformResult newFrom.toPrepend result)) network_)
                                    (Ok newNetwork)
                                    newFrom.results
                        )
                    |> Maybe.withDefault (Err (NodeNotFound to))
            )
        |> Maybe.withDefault (Err (NodeNotFound from))


addResult : NodeId -> ( Structure, Int ) -> Network -> Result GLLParserError Network
addResult nodeId result network =
    Dict.get nodeId network.nodes
        |> Maybe.map
            (\node ->
                let
                    newNode =
                        { node | results = result :: node.results }

                    newNetwork =
                        { network | nodes = Dict.insert nodeId newNode network.nodes }

                    transformedResult =
                        transformResult newNode.toPrepend result
                in
                Set.foldl
                    (\to network_ -> Result.andThen (notify to transformedResult) network_)
                    (Ok newNetwork)
                    newNode.listeners
            )
        |> Maybe.withDefault (Err (NodeNotFound nodeId))


transformResult : Maybe Structure -> ( Structure, Int ) -> ( Structure, Int )
transformResult toPrepend result =
    case toPrepend of
        Nothing ->
            result

        Just toPrepend_ ->
            result
                |> Tuple.mapFirst (\structure -> List [ toPrepend_, structure ])


notify : NodeId -> ( Structure, Int ) -> Network -> Result GLLParserError Network
notify nodeId result network =
    case Dict.get nodeId network.nodes of
        Nothing ->
            Err (NodeNotFound nodeId)

        Just node ->
            let
                ( structure, resultIndex ) =
                    result
            in
            case node.type_ of
                TopNode ->
                    if network.inputLength == resultIndex then
                        network
                            |> addResult nodeId result

                    else
                        -- ignore non-total result. This might change when we implement support for partial = true?
                        Ok network

                NormalNode { strategy, index } ->
                    case strategy of
                        Literal _ ->
                            -- literals are never listeners to anything, so this never happens
                            Ok network

                        Tag tag ->
                            network
                                |> addResult nodeId (Tuple.mapFirst (Structure.Node tag) result)

                        Concatenation _ _ ->
                            -- Concatenation happens automatically because of CatNodes and toPrepend
                            network
                                |> addResult nodeId result

                        Alternation _ _ ->
                            network
                                |> addResult nodeId result

                        Hidden _ ->
                            -- TODO
                            Ok network

                        Optional _ ->
                            -- TODO
                            Ok network

                        ZeroOrMore _ ->
                            -- TODO
                            Ok network

                        OneOrMore _ ->
                            -- TODO
                            Ok network

                        Lookahead _ ->
                            -- TODO
                            Ok network

                CatNode { futureListener, nextStrategy } ->
                    network
                        |> addResult nodeId result
                        |> Result.andThen (addNodeFromStrategy nextStrategy resultIndex (Just futureListener) (Just structure))
                        |> Result.andThen
                            (\( newNetwork, newNodeId, newNode ) ->
                                case matchLiteral newNetwork.input resultIndex newNode of
                                    Nothing ->
                                        Ok newNetwork

                                    Just result_ ->
                                        newNetwork
                                            |> addResult newNodeId result_
                            )


run : String -> Network -> Result GLLParserError Network
run input network =
    network.nodes
        |> Dict.filterMap (\_ node -> matchStartingLiteral input node)
        |> Dict.foldl
            (\nodeId result network_ -> Result.andThen (addResult nodeId result) network_)
            (Ok network)


matchStartingLiteral : String -> Node -> Maybe ( Structure, Int )
matchStartingLiteral input node =
    case node.type_ of
        NormalNode { strategy, index } ->
            if index == 0 then
                matchLiteral input index node

            else
                Nothing

        _ ->
            Nothing


matchLiteral : String -> Int -> Node -> Maybe ( Structure, Int )
matchLiteral input index node =
    case node.type_ of
        NormalNode { strategy } ->
            Strategy.literalValue strategy
                |> Maybe.andThen
                    (\literal ->
                        let
                            length =
                                String.length literal

                            newIndex =
                                index + length
                        in
                        -- TODO do we want to do something like "get first char, check it, then get second char, check it, etc."? Benchmark!
                        if literal == String.slice index newIndex input then
                            Just ( Terminal literal, newIndex )

                        else
                            Nothing
                    )

        _ ->
            Nothing
