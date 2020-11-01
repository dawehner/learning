module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Parser as P
import Test exposing (..)


suite : Test
suite =
    describe "Parsers"
        [ describe "line comment"
            [ test "line" <|
                \_ ->
                    Expect.equal (Ok [ Comment "Test" ]) (executeParseHostfile "# Test")
            ]
        , describe "ipv4"
            [ test "line" <|
                \_ ->
                    Expect.equal
                        (Ok
                            { k1 = 127
                            , k2 = 0
                            , k3 = 0
                            , k4 = 1
                            }
                        )
                        (P.run ip4Parser "127.0.0.1")
            ]
        , describe "ipv6"
            [ test "line" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (List.repeat
                                32
                                0
                            )
                        )
                        (P.run ip6Parser "0000:0000")
            ]
        ]
