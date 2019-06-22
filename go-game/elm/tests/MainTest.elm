module MainTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz
import Main
    exposing
        ( Colour(..)
        , GoGrid
        , Stone(..)
        , getAllOpenPoints
        , gridFromLists
        , hasOneOpenPoint
        )
import Test exposing (..)


fuzzerStone : Fuzz.Fuzzer Stone
fuzzerStone =
    Fuzz.oneOf
        [ Fuzz.constant (Stone Black)
        , Fuzz.constant (Stone White)
        , Fuzz.constant NoStone
        ]


fuzzerListLength : Int -> Fuzz.Fuzzer a -> Fuzz.Fuzzer (List a)
fuzzerListLength n fuzzer =
    if n == 0 then
        Fuzz.constant []

    else
        Fuzz.map2 (::) fuzzer (fuzzerListLength (n - 1) fuzzer)


fuzzerGoGrid : Int -> Fuzz.Fuzzer GoGrid
fuzzerGoGrid size =
    fuzzerListLength size fuzzerStone
        |> fuzzerListLength size
        |> Fuzz.map gridFromLists


testHasOneOpenPoint : Test
testHasOneOpenPoint =
    let
        grid_2_2_empty =
            gridFromLists
                [ [ NoStone, NoStone ]
                , [ NoStone, NoStone ]
                ]
    in
    describe "Tests the hasOneOpenPoint method"
        [ describe "No points"
            [ test "0, 0" <|
                \_ ->
                    Expect.equal True <| hasOneOpenPoint ( 0, 0 ) grid_2_2_empty
            , test "1, 0" <|
                \_ ->
                    Expect.equal True <| hasOneOpenPoint ( 1, 0 ) grid_2_2_empty
            , test "0, 1" <|
                \_ ->
                    Expect.equal True <| hasOneOpenPoint ( 0, 1 ) grid_2_2_empty
            , test "1, 1" <|
                \_ ->
                    Expect.equal True <| hasOneOpenPoint ( 1, 1 ) grid_2_2_empty
            ]
        ]


suite : Test
suite =
    describe "elm go"
        [ testHasOneOpenPoint
        ]
