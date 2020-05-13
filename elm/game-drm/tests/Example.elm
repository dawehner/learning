module Example exposing (..)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Test exposing (..)


suite : Test
suite =
    describe "drm"
        [ describe "pos4InArray"
            [ test "No 4 elements" <|
                \_ ->
                    Expect.equal Nothing
                        (Array.fromList [ Main.Virus Main.Blue, Main.Virus Main.Red, Main.Virus Main.Yellow, Main.Virus Main.Red, Main.Virus Main.Red ]
                            |> Main.pos4InArray
                        )
            , test "4 empties" <|
                \_ ->
                    Expect.equal Nothing (Array.fromList [ Main.Empty, Main.Empty, Main.Empty, Main.Empty ] |> Main.pos4InArray)
            , test "4 elements" <|
                \_ ->
                    Expect.equal (Just ( 1, 4 ))
                        (Array.fromList [ Main.Virus Main.Blue, Main.Virus Main.Red, Main.Virus Main.Red, Main.Virus Main.Red, Main.Virus Main.Red ]
                            |> Main.pos4InArray
                        )
            , test "5 elements" <|
                \_ ->
                    Expect.equal (Just ( 1, 5 ))
                        (Array.fromList
                            [ Main.Virus Main.Blue
                            , Main.Virus Main.Red
                            , Main.Virus Main.Red
                            , Main.Virus Main.Red
                            , Main.Virus Main.Red
                            , Main.Virus Main.Red
                            , Main.Virus Main.Blue
                            ]
                            |> Main.pos4InArray
                        )
            ]
        , describe "pos4InRow"
            [ test "empty area" <|
                \_ ->
                    Expect.equal Nothing
                        (Main.emptyArea
                            |> Main.pos4InRow 0
                        )
            ]
        ]
