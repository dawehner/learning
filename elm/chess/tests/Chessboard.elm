module Chessboard exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (PColor(..), PType(..), addPiece, checkAllowedMove, emptyBoard, initBoard, posToIndex, possibleMovesPos, sortPos)
import Test exposing (..)


suiteSortPos : Test
suiteSortPos =
    describe "sortPos"
        [ test "empty" <|
            \_ ->
                Expect.equal [] <| sortPos []
        , test "example" <|
            \_ ->
                Expect.equal [ ( 3, 4 ), ( 4, 3 ) ] <| sortPos [ ( 3, 4 ), ( 4, 3 ) ]
        ]


suitePossibleMovesPos : Test
suitePossibleMovesPos =
    describe "possibleMovesPos"
        [ describe "Pawns"
            [ test "White Pawns" <|
                \_ ->
                    Expect.equal [ ( 0, 2 ), ( 0, 3 ) ] <| possibleMovesPos ( 0, 1 ) initBoard
            , test "Black Pawns" <|
                \_ ->
                    Expect.equal [ ( 0, 5 ), ( 0, 4 ) ] <| possibleMovesPos ( 0, 6 ) initBoard
            , test "Upper edge" <|
                \_ ->
                    Expect.equal [] <| possibleMovesPos ( 0, 7 ) (initBoard |> addPiece ( 0, 7 ) White Pawn)
            , test "Lower edge" <|
                \_ ->
                    Expect.equal [] <| possibleMovesPos ( 0, 0 ) (initBoard |> addPiece ( 0, 0 ) Black Pawn)
            ]
        , describe "Towers"
            [ test "empty Board" <|
                \_ ->
                    Expect.equal
                        (sortPos
                            [ ( 3, 4 )
                            , ( 3, 5 )
                            , ( 3, 6 )
                            , ( 3, 7 )
                            , ( 3, 2 )
                            , ( 3, 1 )
                            , ( 3, 0 )
                            , ( 4, 3 )
                            , ( 5, 3 )
                            , ( 6, 3 )
                            , ( 7, 3 )
                            , ( 2, 3 )
                            , ( 1, 3 )
                            , ( 0, 3 )
                            ]
                        )
                    <|
                        sortPos <|
                            possibleMovesPos ( 3, 3 ) (emptyBoard |> addPiece ( 3, 3 ) White Tower)
            , test "board with pieces in the way of same colour" <|
                \_ ->
                    Expect.equal
                        (sortPos
                            [ ( 3, 4 )
                            , ( 3, 5 )
                            , ( 3, 6 )
                            , ( 3, 7 )
                            , ( 3, 2 )
                            , ( 4, 3 )
                            , ( 5, 3 )
                            , ( 2, 3 )
                            , ( 1, 3 )
                            , ( 0, 3 )
                            ]
                        )
                    <|
                        sortPos <|
                            possibleMovesPos ( 3, 3 )
                                (emptyBoard
                                    |> addPiece ( 3, 3 ) White Tower
                                    |> addPiece ( 6, 3 ) White Pawn
                                    |> addPiece ( 3, 1 ) White Pawn
                                )
            , test "board with piece in the way of different colour" <|
                \_ ->
                    Expect.equal
                        (sortPos
                            [ ( 1, 0 )
                            , ( 2, 0 )
                            , ( 3, 0 )
                            , ( 4, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 0, 3 )
                            , ( 0, 4 )
                            , ( 0, 5 )
                            , ( 0, 6 )
                            , ( 0, 7 )
                            ]
                        )
                    <|
                        sortPos <|
                            possibleMovesPos ( 0, 0 )
                                (emptyBoard
                                    |> addPiece ( 0, 0 ) White Tower
                                    |> addPiece ( 4, 0 ) Black Pawn
                                )
            , test "board with pieces in the way of different colour" <|
                \_ ->
                    Expect.equal
                        (sortPos
                            [ ( 3, 4 )
                            , ( 3, 5 )
                            , ( 3, 6 )
                            , ( 3, 7 )
                            , ( 3, 2 )
                            , ( 3, 1 )
                            , ( 4, 3 )
                            , ( 5, 3 )
                            , ( 6, 3 )
                            , ( 2, 3 )
                            , ( 1, 3 )
                            , ( 0, 3 )
                            ]
                        )
                    <|
                        sortPos <|
                            possibleMovesPos ( 3, 3 )
                                (emptyBoard
                                    |> addPiece ( 3, 3 ) White Tower
                                    |> addPiece ( 6, 3 ) Black Pawn
                                    |> addPiece ( 3, 1 ) Black Pawn
                                )
            ]
        , describe "Rug"
            [ test "empty Board" <|
                \_ ->
                    Expect.equal
                        (sortPos
                            [ ( 5, 4 )
                            , ( 6, 5 )
                            , ( 7, 6 )
                            , ( 3, 2 )
                            , ( 2, 1 )
                            , ( 1, 0 )
                            , ( 5, 2 )
                            , ( 6, 1 )
                            , ( 7, 0 )
                            , ( 3, 4 )
                            , ( 2, 5 )
                            , ( 1, 6 )
                            , ( 0, 7 )
                            ]
                        )
                    <|
                        sortPos <|
                            possibleMovesPos ( 4, 3 ) (emptyBoard |> addPiece ( 4, 3 ) White Rug)
            , test "less center" <|
                \_ ->
                    Expect.equal
                        (sortPos
                            [ ( 6, 6 )
                            , ( 7, 7 )
                            , ( 4, 4 )
                            , ( 3, 3 )
                            , ( 2, 2 )
                            , ( 1, 1 )
                            , ( 0, 0 )
                            , ( 4, 6 )
                            , ( 3, 7 )
                            , ( 6, 4 )
                            , ( 7, 3 )
                            ]
                        )
                    <|
                        sortPos <|
                            possibleMovesPos ( 5, 5 ) (emptyBoard |> addPiece ( 5, 5 ) White Rug)
            , test "board with pieces in the way" <|
                \_ ->
                    Expect.equal
                        (sortPos
                            [ ( 5, 4 )
                            , ( 6, 5 )
                            , ( 7, 6 )
                            , ( 3, 2 )
                            , ( 2, 1 )
                            , ( 5, 2 )
                            , ( 6, 1 )
                            , ( 3, 4 )
                            , ( 2, 5 )
                            , ( 1, 6 )
                            , ( 0, 7 )
                            ]
                        )
                    <|
                        sortPos <|
                            possibleMovesPos ( 4, 3 )
                                (emptyBoard
                                    |> addPiece ( 4, 3 ) White Rug
                                    |> addPiece ( 1, 0 ) White Pawn
                                    |> addPiece ( 6, 1 ) Black Pawn
                                )
            ]
        ]


suite : Test
suite =
    describe "ChessBoard"
        [ suitePossibleMovesPos
        , suiteSortPos
        , describe "checkAllowedMove"
            [ test "White Pawn moving in the right direction" <|
                \_ ->
                    Expect.equal True
                        (emptyBoard
                            |> addPiece ( 0, 1 ) White Pawn
                            |> checkAllowedMove (posToIndex ( 0, 1 )) (posToIndex ( 0, 2 ))
                        )
            , test "White Pawn moving two in the right direction, base line" <|
                \_ ->
                    Expect.equal True
                        (emptyBoard
                            |> addPiece ( 0, 1 ) White Pawn
                            |> checkAllowedMove (posToIndex ( 0, 1 )) (posToIndex ( 0, 3 ))
                        )
            , test "White Pawn moving two in the right direction, not base line" <|
                \_ ->
                    Expect.equal False
                        (emptyBoard
                            |> addPiece ( 0, 2 ) White Pawn
                            |> checkAllowedMove (posToIndex ( 0, 2 )) (posToIndex ( 0, 4 ))
                        )
            , test "White Pawn moving in the wrong direction" <|
                \_ ->
                    Expect.equal False
                        (emptyBoard
                            |> addPiece ( 0, 2 ) White Pawn
                            |> checkAllowedMove (posToIndex ( 0, 2 )) (posToIndex ( 0, 1 ))
                        )
            , test "Black Pawn moving in the right direction" <|
                \_ ->
                    Expect.equal True
                        (emptyBoard
                            |> addPiece ( 0, 6 ) Black Pawn
                            |> checkAllowedMove (posToIndex ( 0, 6 )) (posToIndex ( 0, 5 ))
                        )
            , test "Black Pawn moving two in the right direction, base line" <|
                \_ ->
                    Expect.equal True
                        (emptyBoard
                            |> addPiece ( 0, 6 ) Black Pawn
                            |> checkAllowedMove (posToIndex ( 0, 6 )) (posToIndex ( 0, 4 ))
                        )
            , test "Black Pawn moving two in the right direction, not base line" <|
                \_ ->
                    Expect.equal False
                        (emptyBoard
                            |> addPiece ( 0, 5 ) Black Pawn
                            |> checkAllowedMove (posToIndex ( 0, 5 )) (posToIndex ( 0, 3 ))
                        )
            , test "Black Pawn moving in the wrong direction" <|
                \_ ->
                    Expect.equal False
                        (emptyBoard
                            |> addPiece ( 0, 6 ) Black Pawn
                            |> checkAllowedMove (posToIndex ( 0, 6 )) (posToIndex ( 0, 7 ))
                        )
            ]
        , describe "posToIndex"
            [ test "0, 0" <|
                \_ ->
                    Expect.equal 0 (posToIndex ( 0, 0 ))
            , test "1, 0" <|
                \_ ->
                    Expect.equal 1 (posToIndex ( 1, 0 ))
            , test "7, 0" <|
                \_ ->
                    Expect.equal 7 (posToIndex ( 7, 0 ))
            , test "0, 1" <|
                \_ ->
                    Expect.equal 8 (posToIndex ( 0, 1 ))
            , test "1, 1" <|
                \_ ->
                    Expect.equal 9 (posToIndex ( 1, 1 ))
            , test "0, 6" <|
                \_ ->
                    Expect.equal 48 (posToIndex ( 0, 6 ))
            , test "0, 7" <|
                \_ ->
                    Expect.equal 56 (posToIndex ( 0, 7 ))
            ]
        ]
