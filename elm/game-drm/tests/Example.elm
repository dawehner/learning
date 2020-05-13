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
        , describe "pos4InColumn"
            [ test "4 reds in a column" <|
                \_ ->
                    Expect.equal (Just ( ( 0, 2 ), 5 ))
                        (Main.emptyArea
                            |> Main.setToArea (Main.Pill Main.Red) ( 0, 0 )
                            |> Main.setToArea (Main.Pill Main.Blue) ( 0, 1 )
                            |> Main.setToArea (Main.Pill Main.Red) ( 0, 2 )
                            |> Main.setToArea (Main.Pill Main.Red) ( 0, 3 )
                            |> Main.setToArea (Main.Pill Main.Red) ( 0, 4 )
                            |> Main.setToArea (Main.Pill Main.Red) ( 0, 5 )
                            |> Main.setToArea (Main.Pill Main.Red) ( 0, 6 )
                            |> Main.pos4InColumn 0
                        )
            ]
        ]
