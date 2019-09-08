module MindmapTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mindmap exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "mindmap" [ nodeLevelTests, getChildrenTests, calculateCoordinatesTest ]


nodeLevelTests : Test
nodeLevelTests =
    describe "nodeLevel"
        [ test "Empty nodes" <|
            \_ ->
                let
                    nodes =
                        emptyNodes

                    node =
                        emptyNode
                            |> withId "test"
                in
                nodeLevel node nodes
                    |> Expect.equal Nothing
        , test "Root node" <|
            \_ ->
                let
                    node =
                        emptyNode
                            |> withId "test"

                    nodes =
                        emptyNodes
                            |> addNode "test" node
                in
                nodeLevel node nodes
                    |> Expect.equal (Just 1)
        , test "Deep deep node" <|
            \_ ->
                let
                    node1 =
                        emptyNode
                            |> withId "root"

                    node2 =
                        emptyNode
                            |> withId "child1"
                            |> withParent "root"

                    node3 =
                        emptyNode
                            |> withId "child2"
                            |> withParent "child1"

                    nodes =
                        emptyNodes
                            |> addNode "test" node1
                            |> addNode "child1" node2
                            |> addNode "child2" node3
                in
                nodeLevel node3 nodes
                    |> Expect.equal (Just 3)
        ]


getChildrenTests : Test
getChildrenTests =
    describe "getChildren"
        [ test "Empty nodes" <|
            \_ ->
                let
                    nodes =
                        emptyNodes

                    node =
                        emptyNode
                            |> withId "test"
                in
                getChildren node nodes
                    |> Expect.equal []
        , test "Root node" <|
            \_ ->
                let
                    node =
                        emptyNode
                            |> withId "test"

                    nodes =
                        emptyNodes
                            |> addNode "test" node
                in
                getChildren node nodes
                    |> Expect.equal []
        , test "Deep deep node" <|
            \_ ->
                let
                    node1 =
                        emptyNode
                            |> withId "root"

                    node2 =
                        emptyNode
                            |> withId "child1"
                            |> withParent "root"

                    node3 =
                        emptyNode
                            |> withId "child2"
                            |> withParent "child1"

                    nodes =
                        emptyNodes
                            |> addNode "test" node1
                            |> addNode "child1" node2
                            |> addNode "child2" node3
                in
                getChildren node2 nodes
                    |> Expect.equal [ node3 ]
        ]


calculateCoordinatesTest : Test
calculateCoordinatesTest =
    let
        node1 =
            emptyNode
                |> withId "root"

        node21 =
            emptyNode
                |> withId "child21"
                |> withParent "root"

        node22 =
            emptyNode
                |> withId "child22"
                |> withParent "root"

        node31 =
            emptyNode
                |> withId "child31"
                |> withParent "child21"

        node32 =
            emptyNode
                |> withId "child32"
                |> withParent "child22"

        nodes =
            emptyNodes
                |> addNodeWithId node1
                |> addNodeWithId node21
                |> addNodeWithId node22
                |> addNodeWithId node31
                |> addNodeWithId node32
    in
    describe "calculateCoordinates"
        [ test "calculateCoordinates for first level child" <|
            \_ ->
                calculateCoordinates node21 node1 nodes
                    |> Expect.equal { x = 200, y = 60 }
        , test "calculateCoordinates for first level child (2)" <|
            \_ ->
                calculateCoordinates node22 node1 nodes
                    |> Expect.equal { x = 200, y = 60 }
        , test "calculateCoordinates adding on level 2" <|
            \_ ->
                calculateCoordinates node31 node21 nodes
                    |> Expect.equal { x = 200, y = -120 }
        , test "calculateCoordinates adding on level 2 (2)" <|
            \_ ->
                calculateCoordinates node32 node22 nodes
                    |> Expect.equal { x = 200, y = -120 }
        ]
