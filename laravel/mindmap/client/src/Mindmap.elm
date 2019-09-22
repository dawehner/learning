module Mindmap exposing (..)

import Dict
import Element exposing (onLeft)


type alias Node =
    { id : String
    , parent : Maybe String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    , locked : Bool
    , text : String
    }


emptyNode : Node
emptyNode =
    { id = "empty"
    , parent = Just "no-parent"
    , x = 0
    , y = 0
    , width = 50
    , height = 50
    , locked = False
    , text = ""
    }


setRoot : Node -> Node
setRoot node =
    { node | parent = Nothing }


mindMapGenerator : Nodes
mindMapGenerator =
    Debug.todo "not implemented"


withId : String -> Node -> Node
withId id node =
    { node | id = id }


withText : String -> Node -> Node
withText text node =
    { node | text = text }


withParent : String -> Node -> Node
withParent id node =
    { node | parent = Just id }


withCoords : Float -> Float -> Node -> Node
withCoords x y node =
    { node | x = x, y = y }


type alias Coords =
    { x : Float, y : Float }


getCoords : Node -> Coords
getCoords { x, y } =
    { x = x, y = y }


type Nodes
    = Nodes (Dict.Dict String Node)


nodesToList : Nodes -> List Node
nodesToList (Nodes nodes) =
    Dict.values nodes


nodesToDict : Nodes -> Dict.Dict String Node
nodesToDict (Nodes nodes) =
    nodes


emptyNodes =
    Nodes <| Dict.empty


addNode id node (Nodes nodes) =
    Nodes (Dict.insert id node nodes)


addNodeWithId : Orientation -> Node -> Nodes -> Nodes
addNodeWithId orientation node nodes =
    case getParent node nodes of
        Just parent ->
            let
                newX =
                    if orientation == Left then
                        parent.x - 200

                    else if orientation == Right then
                        parent.x + 200

                    else
                        parent.x
            in
            calculateCoordinates node parent nodes
                |> (\{ x, y } -> node |> withCoords newX y)
                |> (\node_ -> Dict.insert node.id node_ (nodesToDict nodes))
                |> Nodes

        Nothing ->
            nodesToDict nodes
                |> Dict.insert node.id node
                |> Nodes


getParent : Node -> Nodes -> Maybe Node
getParent node nodes =
    node.parent
        |> Maybe.andThen (\parent -> Dict.get parent (nodesToDict nodes))


getParentById : String -> Nodes -> Maybe Node
getParentById id (Nodes nodes) =
    Dict.get id nodes
        |> Maybe.andThen .parent
        |> Maybe.andThen (\parent -> Dict.get parent nodes)


getRoot : Nodes -> Maybe Node
getRoot (Nodes nodes) =
    Dict.values nodes
        |> Debug.log "nodes"
        |> List.filter (\node -> node.parent == Nothing)
        |> List.head


isRoot : Node -> Bool
isRoot node =
    node.parent == Nothing


nodeLevel : Node -> Nodes -> Maybe Int
nodeLevel node nodes =
    nodeLevelId node.id nodes


nodeLevelId : String -> Nodes -> Maybe Int
nodeLevelId id (Nodes nodes) =
    if Dict.member id nodes then
        case getParentById id (Nodes nodes) of
            Nothing ->
                Just 1

            Just parent ->
                case nodeLevelId parent.id (Nodes nodes) of
                    Nothing ->
                        Just 2

                    Just depth ->
                        Just (2 + depth)

    else
        Nothing


getChildren : Node -> Nodes -> List Node
getChildren node (Nodes nodes) =
    Dict.values nodes
        |> List.filter (\node_ -> node_.parent == Just node.id)


getSiblings : Node -> Nodes -> List Node
getSiblings node (Nodes nodes) =
    case node.parent of
        Nothing ->
            []

        Just parent ->
            Dict.values nodes
                |> List.filter (\node_ -> node_.id /= node.id && node_.parent == Just parent)


type Orientation
    = Left
    | Right
    | Root


calculateOrientation : Node -> Nodes -> Orientation
calculateOrientation node nodes =
    case getRoot nodes of
        Just rootNode ->
            if node.x < rootNode.x then
                Left

            else
                Right

        Nothing ->
            Root


calculateYCoordinate : Float -> List Node -> Float
calculateYCoordinate parentY siblings =
    getLowerNode siblings
        |> Maybe.map (\node -> node.y + 60)
        |> Maybe.withDefault (parentY - 120)


getLowerNode : List Node -> Maybe Node
getLowerNode siblings =
    List.sortBy .y siblings
        |> List.reverse
        |> List.head


calculateCoordinates : Node -> Node -> Nodes -> { x : Float, y : Float }
calculateCoordinates node parent nodes =
    let
        parentX =
            parent.x

        parentY =
            parent.y

        siblings =
            getSiblings node nodes

        allNodes =
            nodesToList nodes
    in
    if parent.parent == Nothing then
        let
            leftNodes =
                List.filter (\sibling -> calculateOrientation sibling nodes == Left) allNodes

            rightNodes =
                List.filter (\sibling -> calculateOrientation sibling nodes == Right) allNodes
        in
        if List.length leftNodes <= List.length rightNodes then
            { x = parentX - 200
            , y = calculateYCoordinate parentY leftNodes
            }

        else
            { x = parentX + 200
            , y = calculateYCoordinate parentY rightNodes
            }

    else if calculateOrientation node nodes == Left then
        { x = parentX - 200
        , y = calculateYCoordinate parentY siblings
        }

    else
        { x = parentX + 200
        , y = calculateYCoordinate parentY siblings
        }


maxNodes : Nodes -> { x : Float, y : Float }
maxNodes (Nodes nodes) =
    List.foldl
        (\node { x, y } ->
            { x = abs node.x |> max x
            , y = abs node.y |> max y
            }
        )
        { x = 0.0, y = 0.0 }
        (Dict.values nodes)


exampleNodes =
    Nodes Dict.empty
        |> addNodeWithId Root
            (emptyNode
                |> withText "root"
                |> withId "root"
                |> withCoords 400 300
                |> setRoot
            )
        |> addNodeWithId Left
            (emptyNode
                |> withText "child 11"
                |> withId "child11"
                |> withParent "root"
            )
        |> addNodeWithId Left
            (emptyNode
                |> withText "child 12"
                |> withId "child12"
                |> withParent "root"
            )
        |> addNodeWithId Right
            (emptyNode
                |> withText "child 21"
                |> withId "child21"
                |> withParent "root"
            )
        |> addNodeWithId Right
            (emptyNode
                |> withText "child 22"
                |> withId "child22"
                |> withParent "root"
            )
