module Chessboard exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (PColor(..), PType(..), addPiece, checkAllowedMove, emptyBoard, posToIndex)
import Test exposing (..)

suite : Test
suite =
    describe "ChessBoard" [
        describe "checkAllowedMove" [
            test "White Pawn moving in the right direction" <|
                \_ ->
                  Expect.equal True (
                    emptyBoard
                    |> addPiece (0, 1) White Pawn
                    |> checkAllowedMove (posToIndex (0, 1)) (posToIndex (0, 2))
                  )
            , test "White Pawn moving two in the right direction, base line" <|
                \_ ->
                  Expect.equal True (
                    emptyBoard
                    |> addPiece (0, 1) White Pawn
                    |> checkAllowedMove (posToIndex (0, 1)) (posToIndex (0, 3))
                  )
            , test "White Pawn moving two in the right direction, not base line" <|
                \_ ->
                  Expect.equal False (
                    emptyBoard
                    |> addPiece (0, 2) White Pawn
                    |> checkAllowedMove (posToIndex (0, 2)) (posToIndex (0, 4))
                  )
            , test "White Pawn moving in the wrong direction" <|
                \_ ->
                  Expect.equal False (
                    emptyBoard
                    |> addPiece (0, 2) White Pawn
                    |> checkAllowedMove (posToIndex (0, 2)) (posToIndex (0, 1))
                  )
            , test "Black Pawn moving in the right direction" <|
                \_ ->
                  Expect.equal True (
                    emptyBoard
                    |> addPiece (0, 6) Black Pawn
                    |> checkAllowedMove (posToIndex (0, 6)) (posToIndex (0, 5))
                  )
            , test "Black Pawn moving two in the right direction, base line" <|
                \_ ->
                  Expect.equal True (
                    emptyBoard
                    |> addPiece (0, 6) Black Pawn
                    |> checkAllowedMove (posToIndex (0, 6)) (posToIndex (0, 4))
                  )
            , test "Black Pawn moving two in the right direction, not base line" <|
                \_ ->
                  Expect.equal False (
                    emptyBoard
                    |> addPiece (0, 5) Black Pawn
                    |> checkAllowedMove (posToIndex (0, 5)) (posToIndex (0, 3))
                  )
            , test "Black Pawn moving in the wrong direction" <|
                \_ ->
                  Expect.equal False (
                    emptyBoard
                    |> addPiece (0, 6) Black Pawn
                    |> checkAllowedMove (posToIndex (0, 6)) (posToIndex (0, 7))
                  )
        ]
        ,
        describe "posToIndex" [
            test "0, 0" <|
                \_ ->
                  Expect.equal 0 (posToIndex (0, 0))
            , test "1, 0" <|
                \_ ->
                  Expect.equal 1 (posToIndex (1, 0))
            , test "7, 0" <|
                \_ ->
                  Expect.equal 7 (posToIndex (7, 0))
            , test "0, 1" <|
                \_ ->
                  Expect.equal 8 (posToIndex (0, 1))
            , test "1, 1" <|
                \_ ->
                  Expect.equal 9 (posToIndex (1, 1))
            , test "0, 6" <|
                \_ ->
                  Expect.equal 48 (posToIndex (0, 6))
            , test "0, 7" <|
                \_ ->
                  Expect.equal 56 (posToIndex (0, 7))
        ]
    ]
