module Main exposing (..)

import Array
import GraphicSVG as G
import GraphicSVG.App
import Html
import Html.Attributes


type PColor
    = Black
    | White
    | None


pColorToColor : PColor -> G.Color
pColorToColor color =
    case color of
        Black ->
            G.black

        White ->
            G.lightGray

        None ->
            G.white


type PType
    = Pawn
    | Tower
    | Horse
    | Rug
    | Queen
    | King
    | PNone


type alias Piece =
    ( PColor, PType )


type alias Model =
    { board : Board
    , currentPiece : Maybe Int
    , currentPlayer : PColor
    }


type alias Board =
    Array.Array Piece

emptyBoard : Board
emptyBoard = Array.repeat 64 ( None, PNone )

initBoard : Board
initBoard =
    emptyBoard
        |> Array.set 0 ( White, Tower )
        |> Array.set 1 ( White, Horse )
        |> Array.set 2 ( White, Rug )
        |> Array.set 3 ( White, Queen )
        |> Array.set 4 ( White, King )
        |> Array.set 5 ( White, Rug )
        |> Array.set 6 ( White, Horse )
        |> Array.set 7 ( White, Tower )
        |> Array.set 8 ( White, Pawn )
        |> Array.set 9 ( White, Pawn )
        |> Array.set 10 ( White, Pawn )
        |> Array.set 11 ( White, Pawn )
        |> Array.set 12 ( White, Pawn )
        |> Array.set 13 ( White, Pawn )
        |> Array.set 14 ( White, Pawn )
        |> Array.set 15 ( White, Pawn )
        |> Array.set 48 ( Black, Pawn )
        |> Array.set 49 ( Black, Pawn )
        |> Array.set 50 ( Black, Pawn )
        |> Array.set 51 ( Black, Pawn )
        |> Array.set 52 ( Black, Pawn )
        |> Array.set 53 ( Black, Pawn )
        |> Array.set 54 ( Black, Pawn )
        |> Array.set 55 ( Black, Pawn )
        |> Array.set 56 ( Black, Tower )
        |> Array.set 57 ( Black, Horse )
        |> Array.set 58 ( Black, Rug )
        |> Array.set 59 ( Black, Queen )
        |> Array.set 60 ( Black, King )
        |> Array.set 61 ( Black, Rug )
        |> Array.set 62 ( Black, Horse )
        |> Array.set 63 ( Black, Tower )

addPiece : Pos -> PColor -> PType -> Board -> Board
addPiece pos color piece board =
    board
    |> Array.set (posToIndex pos) (color, piece)

type alias Pos =
    ( Int, Int )


indexToPos : Int -> Pos
indexToPos i =
    ( modBy 8 i, i // 8 )

posToIndex : Pos -> Int
posToIndex (x, y) = x + 8 * y

type Msg
    = Tick Float GraphicSVG.App.GetKeyState
    | ClickPiece Int


init : Model
init =
    { board = initBoard
    , currentPiece = Nothing
    , currentPlayer = White
    }

checkPosIsMate : PColor -> Board -> Bool
checkPosIsMate color board = False


checkAllowedMove : Int -> Int -> Board -> Bool
checkAllowedMove prev next board =
    let
        ( x1, y1 ) =
            indexToPos prev

        ( x2, y2 ) =
            indexToPos next
    in
    case Array.get prev board of
        Nothing ->
            False

        Just ( pc, pt ) ->
            case pt of
                Pawn ->
                    if pc == White then
                        if y1 >= y2 then False
                        else if y2 - y1 == 1 then True
                        else if y1 == 1 && y2 == 3 then True
                        else False
                    else if pc == Black then
                        if y2 >= y1 then False
                        else if y1 - y2 == 1 then True
                        else if y1 == 6 && y2 == 4 then True
                        else False
                    else True

                Tower ->
                    False

                Horse ->
                    False

                Rug ->
                    False

                Queen ->
                    False

                King ->
                    False

                PNone ->
                    False


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick _ ( keys, _, _ ) ->
            model

        ClickPiece next ->
            let
                nextPlayer =
                    if model.currentPlayer == White then
                        Black

                    else
                        White
            in
            case model.currentPiece of
                Just prev ->
                    case Array.get prev model.board of
                        Just ( color1, pType1 ) ->
                            --if color2 /= color then
                            --    -- handle errors?
                            --    model
                            --
                            --else
                            case Array.get next model.board of
                                Just ( None, _ ) ->
                                    if checkAllowedMove prev next model.board then
                                        { model
                                            | board =
                                                Array.set prev ( None, PNone ) model.board
                                                    |> Array.set next ( color1, pType1 )
                                            , currentPiece = Nothing
                                            , currentPlayer = nextPlayer
                                        }
                                    else
                                        model

                                Just ( color2, pType2 ) ->
                                    -- Beat someone
                                    if color1 /= color2 then
                                        { model
                                            | board =
                                                Array.set prev ( None, PNone ) model.board
                                                    |> Array.set next ( color1, pType1 )
                                            , currentPiece = Nothing
                                            , currentPlayer = nextPlayer
                                        }

                                    else
                                        { model
                                            | currentPiece = Nothing
                                            , currentPlayer = nextPlayer
                                        }

                                -- move to empty pos
                                Nothing ->
                                    { model
                                        | board =
                                            Array.set prev ( None, PNone ) model.board
                                                |> Array.set next ( color1, pType1 )
                                        , currentPiece = Nothing
                                        , currentPlayer = nextPlayer
                                    }

                        Nothing ->
                            { model
                                | currentPiece = Nothing
                                , currentPlayer = nextPlayer
                            }

                Nothing ->
                    case Array.get next model.board of
                        Nothing -> model
                        Just (None, PNone) -> model
                        Just _ ->
                            { model
                                | currentPiece = Just next
                                , currentPlayer = nextPlayer
                            }


drawPawn : PColor -> G.Shape userMsg
drawPawn color =
    G.html
        45
        45
        (Html.img
            [ (case color of
                Black ->
                    "/static/Chess_pdt45.svg"

                White ->
                    "/static/Chess_plt45.svg"

                None ->
                    ""
              )
                |> Html.Attributes.src
            ]
            []
        )
        |> G.scale 0.75


drawTower : PColor -> G.Shape userMsg
drawTower color =
    G.html
        45
        45
        (Html.img
            [ (case color of
                Black ->
                    "/static/Chess_rdt45.svg"

                White ->
                    "/static/Chess_rlt45.svg"

                None ->
                    ""
              )
                |> Html.Attributes.src
            ]
            []
        )
        |> G.scale 0.75


drawHorse : PColor -> G.Shape userMsg
drawHorse color =
    G.html
        45
        45
        (Html.img
            [ (case color of
                Black ->
                    "/static/Chess_ndt45.svg"

                White ->
                    "/static/Chess_nlt45.svg"

                None ->
                    ""
              )
                |> Html.Attributes.src
            ]
            []
        )
        |> G.scale 0.75


drawRug : PColor -> G.Shape userMsg
drawRug color =
    G.html
        45
        45
        (Html.img
            [ (case color of
                Black ->
                    "/static/Chess_bdt45.svg"

                White ->
                    "/static/Chess_blt45.svg"

                None ->
                    ""
              )
                |> Html.Attributes.src
            ]
            []
        )
        |> G.scale 0.75


drawQueen : PColor -> G.Shape userMsg
drawQueen color =
    G.html
        45
        45
        (Html.img
            [ (case color of
                Black ->
                    "/static/Chess_qdt45.svg"

                White ->
                    "/static/Chess_qlt45.svg"

                None ->
                    ""
              )
                |> Html.Attributes.src
            ]
            []
        )
        |> G.scale 0.75


drawKing : PColor -> G.Shape userMsg
drawKing color =
    G.html
        45
        45
        (Html.img
            [ (case color of
                Black ->
                    "/static/Chess_kdt45.svg"

                White ->
                    "/static/Chess_klt45.svg"

                None ->
                    ""
              )
                |> Html.Attributes.src
            ]
            []
        )
        |> G.scale 0.75


drawPieceAt ( x, y ) piece =
    piece |> G.move ( 30 * x, 30 * y )


chessBoard : List (G.Shape userMsg)
chessBoard =
    (List.range 0 7
        |> List.map
            (\x ->
                List.range 0 7
                    |> List.map
                        (\y ->
                            if modBy 2 (x + y) == 0 then
                                G.square 30
                                    |> G.filled G.darkGray
                                    |> G.move
                                        ( 30 * toFloat x + 15
                                        , 30 * toFloat y + 15
                                        )

                            else
                                G.square 30
                                    |> G.filled G.yellow
                                    |> G.move
                                        ( 30 * toFloat x + 15
                                        , 30 * toFloat y + 15
                                        )
                        )
            )
        |> List.foldl (++) []
    )
        ++ [ G.line ( 0, 0 ) ( 240, 0 )
                |> G.outlined (G.solid 1) G.black
           , G.line ( 240, 0 ) ( 240, 240 )
                |> G.outlined (G.solid 1) G.black
           , G.line ( 0, 0 ) ( 0, 240 )
                |> G.outlined (G.solid 1) G.black
           , G.line ( 0, 240 ) ( 240, 240 )
                |> G.outlined (G.solid 1) G.black
           ]


drawPiece : Int -> Piece -> G.Shape Msg
drawPiece i piece =
    let
        ( x, y ) =
            indexToPos i
    in
    case piece of
        ( c, Pawn ) ->
            drawPawn c
                |> drawPieceAt ( toFloat x, toFloat y + 1 )
                |> G.notifyTap (ClickPiece i)

        ( c, Tower ) ->
            drawTower c
                |> drawPieceAt ( toFloat x, toFloat y + 1 )
                |> G.notifyTap (ClickPiece i)

        ( c, Horse ) ->
            drawHorse c
                |> drawPieceAt ( toFloat x, toFloat y + 1 )
                |> G.notifyTap (ClickPiece i)

        ( c, Rug ) ->
            drawRug c
                |> drawPieceAt ( toFloat x, toFloat y + 1 )
                |> G.notifyTap (ClickPiece i)

        ( c, Queen ) ->
            drawQueen c
                |> drawPieceAt ( toFloat x, toFloat y + 1 )
                |> G.notifyTap (ClickPiece i)

        ( c, King ) ->
            drawKing c
                |> drawPieceAt ( toFloat x, toFloat y + 1 )
                |> G.notifyTap (ClickPiece i)

        ( _, _ ) ->
            G.square 30
                --|> G.filled G.red
                --|> G.addOutline (G.solid 5) G.white
                |> G.ghost
                |> G.move ( 30 * toFloat x + 15, 30 * toFloat y + 15 )
                |> G.notifyTap (ClickPiece i)


view : Model -> G.Collage Msg
view model =
    G.collage 500
        500
        (chessBoard
            ++ (Array.indexedMap
                    (\i piece ->
                        drawPiece i piece
                    )
                    model.board
                    |> Array.toList
               )
           ++ (
                case model.currentPiece of
                    Just i ->
                        let
                            ( x, y ) = indexToPos i
                        in
                            [G.square 30
                                |> G.outlined (G.solid 1) (G.red)
                                |> drawPieceAt ( toFloat x + 0.5, toFloat y + 0.5 )
                            ]
                    Nothing -> []
           )
        )


main : GraphicSVG.App.NotificationsApp Model Msg
main =
    GraphicSVG.App.notificationsApp
        { model = init
        , update = update
        , view = view
        }
