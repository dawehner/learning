module Mindmap exposing (..)

import Dict



--type alias MindmapTest = {
--  text : String,
--  x : Float,
--  y : Float,
--  left: MindmapChildren,
--  right: MindMapChildren
--}


type alias Node =
    { id : String
    , parent : Maybe String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    , locked : Bool
    }


emptyNode : Node
emptyNode =
    { id = "empty"
    , parent = Just "no-parent"
    , x = 0
    , y = 0
    , width = 0
    , height = 0
    , locked = False
    }


withId : String -> Node -> Node
withId id node =
    { node | id = id }

withParent : String -> Node -> Node
withParent id node =
    { node | parent = Just id }


type Nodes
    = Nodes (Dict.Dict String Node)


emptyNodes =
    Nodes <| Dict.empty


addNode id node (Nodes nodes) =
    Nodes (Dict.insert id node nodes)

getParent : String -> Nodes -> Maybe Node
getParent id (Nodes nodes) =
    Dict.get id nodes
    |> Maybe.andThen .parent
    |> Maybe.andThen (\parent -> Dict.get parent nodes)

getRoot : Nodes -> Maybe Node
getRoot (Nodes nodes) =
    Dict.values nodes
    |> List.filter (\node -> node.parent == Nothing)
    |> List.head

nodeLevel : Node -> Nodes -> Maybe Int
nodeLevel node nodes =
    nodeLevelId node.id nodes

nodeLevelId : String -> Nodes -> Maybe Int
nodeLevelId id (Nodes nodes) =
    if (Dict.member id nodes) then
        case getParent id (Nodes nodes) of
            Nothing -> Just 1
            Just parent -> case nodeLevelId parent.id (Nodes nodes) of
                Nothing -> Just 2
                Just depth -> Just (2 + depth)
    else
      Nothing

getChildren : Node -> Nodes -> List Node
getChildren node (Nodes nodes) =
    Dict.values nodes
    |> List.filter (\node_ -> node_.parent == Just node.id)

