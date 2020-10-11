module Main exposing (..)

import Array
import Array.Extra
import GraphicSVG as G
import GraphicSVG.App
import Html
import Html.Attributes
import Utility exposing (elemIndex)


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
    , history : List HistoryEntry
    }


type HistoryEntry
    = HistoryEntry Pos Pos


type alias Board =
    Array.Array Piece


emptyBoard : Board
emptyBoard =
    Array.repeat 64 ( None, PNone )


exampleBoard : Board
exampleBoard =
    emptyBoard
        |> addPiece ( 0, 1 ) White Pawn
        |> addPiece ( 1, 2 ) Black Pawn


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
        |> Array.set (posToIndex pos) ( color, piece )


type alias Pos =
    ( Int, Int )


indexToPos : Int -> Pos
indexToPos i =
    ( modBy 8 i, i // 8 )


posToIndex : Pos -> Int
posToIndex ( x, y ) =
    x + 8 * y


getAtPos : Pos -> Board -> Piece
getAtPos pos board =
    Array.get (posToIndex pos) board
        |> Maybe.withDefault ( None, PNone )


type Msg
    = Tick Float GraphicSVG.App.GetKeyState
    | ClickPiece Int


init : Model
init =
    { board =
        initBoard
    , currentPiece = Nothing
    , currentPlayer = White
    , history = []
    }


checkPosIsMate : PColor -> Board -> Maybe Pos
checkPosIsMate color board =
    let
        posKing =
            elemIndex ( revertColor color, King ) board
            |> Maybe.map indexToPos
    in
        Maybe.andThen
            (\pos ->
                -- For each position on the board get the possible moves
                Array.indexedMap
                    (\i ( c, p ) ->
                        if c == color then
                            possibleMovesPos (indexToPos i) board

                        else
                            []
                    )
                    board
                    -- Now for each piece we have their moves so we can
                    -- see whether they can hit the king.
                    |> Array.map
                        (\possibleMoves ->
                            List.member pos possibleMoves
                        )
                    |> elemIndex True
                    |> Maybe.map indexToPos
            ) posKing


checkAllowedMove : Int -> Int -> Board -> Bool
checkAllowedMove prev next board =
    let
        pos1 =
            indexToPos prev

        ( x1, y1 ) =
            pos1

        pos2 =
            indexToPos next

        moves =
            possibleMovesPos pos1 board
    in
    List.member pos2 moves


sortPos : List Pos -> List Pos
sortPos xs =
    List.sortWith
        (\( x1, y1 ) ( x2, y2 ) ->
            if x1 < x2 then
                LT

            else if x1 > x2 then
                GT

            else if y1 < y2 then
                LT

            else if y1 > y2 then
                GT

            else
                EQ
        )
        xs


filterOutsideBoard : List Pos -> List Pos
filterOutsideBoard =
    List.filter (\( x, y ) -> x >= 0 && x <= 7 && y >= 0 && y <= 7)


revertColor : PColor -> PColor
revertColor c =
    case c of
        None ->
            None

        White ->
            Black

        Black ->
            White


emptyOrEqual : PColor -> Board -> Pos -> Bool
emptyOrEqual color board pos =
    let
        ( c2, p2 ) =
            getAtPos pos board
    in
    if c2 == None || c2 == color then
        True

    else
        False


filterCollison : PColor -> Board -> List Pos -> List Pos
filterCollison c board ps =
    let
        collisionMemo list memo =
            case list of
                [] ->
                    List.reverse memo

                x :: xs ->
                    let
                        ( c2, _ ) =
                            getAtPos x board
                    in
                    if c2 == c then
                        -- same colour
                        List.reverse memo

                    else if c2 == None then
                        -- nothing
                        collisionMemo xs (x :: memo)

                    else
                        -- different colour
                        List.reverse (x :: memo)
    in
    collisionMemo ps []


filterCollisonPawn : PColor -> Pos -> Board -> List Pos -> List Pos
filterCollisonPawn c pos board moves =
    let
        ( x1, y1 ) =
            pos

        collisionMemo list memo =
            case list of
                [] ->
                    List.reverse memo

                p :: ps ->
                    let
                        ( c2, _ ) =
                            getAtPos p board

                        ( x2, y2 ) =
                            p
                    in
                    if c2 == None then
                        -- nothing
                        collisionMemo ps (p :: memo)

                    else if c /= c2 then
                        if abs (x1 - x2) == 1 then
                            collisionMemo ps (p :: memo)

                        else
                            List.reverse memo

                    else
                        -- same colour
                        List.reverse memo
    in
    collisionMemo moves []


filterCollisonHorse : PColor -> Board -> List Pos -> List Pos
filterCollisonHorse c board ps =
    List.filter
        (\p ->
            let
                ( c2, _ ) =
                    getAtPos p board
            in
            if c == c2 then
                False

            else
                True
        )
        ps


possibleMovesPos : Pos -> Board -> List Pos
possibleMovesPos pos board =
    let
        ( x1, y1 ) =
            pos

        movesTower c =
            (List.range (x1 + 1) 7
                |> List.map (\x -> ( x, y1 ))
                |> filterCollison c board
            )
                ++ (List.range 0 (x1 - 1)
                        |> List.reverse
                        |> List.map (\x -> ( x, y1 ))
                        |> filterCollison c board
                   )
                ++ (List.range (y1 + 1) 7
                        |> List.map (\y -> ( x1, y ))
                        |> filterCollison c board
                   )
                ++ (List.range 0 (y1 - 1)
                        |> List.reverse
                        |> List.map (\y -> ( x1, y ))
                        |> filterCollison c board
                   )

        movesRug c =
            (List.range (x1 + 1) 7
                |> List.map (\x -> ( x1 + (x - x1), y1 + (x - x1) ))
                |> filterCollison c board
            )
                -- x-,y-
                ++ (List.range 0 (x1 - 1)
                        |> List.reverse
                        |> List.map (\x -> ( x1 - (x1 - x), y1 - (x1 - x) ))
                        |> filterCollison c board
                   )
                ++ (List.range (x1 + 1) 7
                        |> List.map (\x -> ( x1 + (x - x1), y1 - (x - x1) ))
                        |> filterCollison c board
                   )
                ++ (List.range 0 (x1 - 1)
                        |> List.reverse
                        |> List.map (\x -> ( x1 - (x1 - x), y1 + (x1 - x) ))
                        |> filterCollison c board
                   )

        possiblePos =
            case Array.get (posToIndex pos) board of
                Nothing ->
                    []

                Just ( c, Pawn ) ->
                    if c == White then
                        (if y1 == 1 then
                            [ ( x1, y1 + 1 ), ( x1, y1 + 2 ) ]

                         else
                            [ ( x1, y1 + 1 ) ]
                        )
                            ++ (if Tuple.first (getAtPos ( x1 - 1, y1 + 1 ) board) == Black then
                                    [ ( x1 - 1, y1 + 1 ) ]

                                else
                                    []
                               )
                            ++ (if Tuple.first (getAtPos ( x1 + 1, y1 + 1 ) board) == Black then
                                    [ ( x1 + 1, y1 + 1 ) ]

                                else
                                    []
                               )
                            |> filterCollisonPawn c ( x1, y1 ) board

                    else if c == Black then
                        (if y1 == 6 then
                            [ ( x1, y1 - 1 ), ( x1, y1 - 2 ) ]

                         else
                            [ ( x1, y1 - 1 ) ]
                        )
                            ++ (if Tuple.first (getAtPos ( x1 - 1, y1 - 1 ) board) == White then
                                    [ ( x1 - 1, y1 - 1 ) ]

                                else
                                    []
                               )
                            ++ (if Tuple.first (getAtPos ( x1 + 1, y1 - 1 ) board) == White then
                                    [ ( x1 + 1, y1 - 1 ) ]

                                else
                                    []
                               )
                            |> filterCollisonPawn c ( x1, y1 ) board

                    else
                        []

                Just ( c, Tower ) ->
                    movesTower c

                Just ( c, Rug ) ->
                    -- x+,y+
                    movesRug c

                Just ( c, Horse ) ->
                    [ ( x1 - 1, y1 + 2 )
                    , ( x1 - 2, y1 + 1 )
                    , ( x1 + 1, y1 + 2 )
                    , ( x1 + 2, y1 + 1 )
                    , ( x1 - 1, y1 - 2 )
                    , ( x1 - 2, y1 - 1 )
                    , ( x1 + 1, y1 - 2 )
                    , ( x1 + 2, y1 - 1 )
                    ]
                        |> filterCollisonHorse c board

                Just ( c, Queen ) ->
                    movesTower c ++ movesRug c

                _ ->
                    []
    in
    possiblePos |> filterOutsideBoard


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
                    let
                        nextHistory =
                            HistoryEntry (indexToPos prev) (indexToPos next)
                    in
                    case Array.get prev model.board of
                        Just ( color1, pType1 ) ->
                            case Array.get next model.board of
                                Just ( None, _ ) ->
                                    if checkAllowedMove prev next model.board then
                                        { model
                                            | board =
                                                Array.set prev ( None, PNone ) model.board
                                                    |> Array.set next ( color1, pType1 )
                                            , currentPiece = Nothing
                                            , currentPlayer = nextPlayer
                                            , history = nextHistory :: model.history
                                        }

                                    else
                                        model

                                Just ( color2, pType2 ) ->
                                    -- Beat someone
                                    if checkAllowedMove prev next model.board then
                                        if color1 /= color2 then
                                            { model
                                                | board =
                                                    Array.set prev ( None, PNone ) model.board
                                                        |> Array.set next ( color1, pType1 )
                                                , currentPiece = Nothing
                                                , currentPlayer = nextPlayer
                                                , history = nextHistory :: model.history
                                            }

                                        else
                                            { model
                                                | currentPiece = Nothing
                                                , currentPlayer = nextPlayer
                                            }

                                    else
                                        model

                                -- move to empty pos
                                Nothing ->
                                    { model
                                        | board =
                                            Array.set prev ( None, PNone ) model.board
                                                |> Array.set next ( color1, pType1 )
                                        , currentPiece = Nothing
                                        , currentPlayer = nextPlayer
                                        , history = nextHistory :: model.history
                                    }

                        Nothing ->
                            { model
                                | currentPiece = Nothing
                                , currentPlayer = nextPlayer
                            }

                Nothing ->
                    case Array.get next model.board of
                        Nothing ->
                            model

                        Just ( None, PNone ) ->
                            model

                        Just ( c, _ ) ->
                            if c == model.currentPlayer then
                                { model
                                    | currentPiece = Just next
                                }

                            else
                                model


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


historyEntryToLabel : HistoryEntry -> ( ( String, String ), ( String, String ) )
historyEntryToLabel (HistoryEntry p1 p2) =
    ( posToPosLabel p1, posToPosLabel p2 )


posToPosLabel : Pos -> ( String, String )
posToPosLabel ( x, y ) =
    ( case x of
        0 ->
            "A"

        1 ->
            "B"

        2 ->
            "C"

        3 ->
            "D"

        4 ->
            "E"

        5 ->
            "F"

        6 ->
            "G"

        7 ->
            "H"

        _ ->
            "_"
    , String.fromInt (y + 1)
    )


historyLog : List HistoryEntry -> List (G.Shape userMsg)
historyLog hs =
    List.map historyEntryToLabel hs
        |> List.map
            (\( ( x1, y1 ), ( x2, y2 ) ) ->
                G.text (x1 ++ y1 ++ "-" ++ x2 ++ y2) |> G.filled G.black
            )
        |> List.indexedMap (\i x -> G.move ( 40 * toFloat i, -50 ) x)


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
        ++ ([ "A", "B", "C", "D", "E", "F", "G", "H" ]
                |> List.indexedMap (\i s -> G.text s |> G.filled G.black |> G.move ( 30 * toFloat i + 8, -10 ))
           )
        ++ (List.range 1 8
                |> List.map (\i -> G.text (String.fromInt i) |> G.filled G.black |> G.move ( -10, 30 * toFloat (i - 1) + 8 ))
           )


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
            ++ historyLog model.history
            ++ showCheckMate model
            ++ (case model.currentPiece of
                    Just i ->
                        let
                            ( x, y ) =
                                indexToPos i
                        in
                        [ G.square 30
                            |> G.outlined (G.solid 1) G.red
                            |> drawPieceAt ( toFloat x + 0.5, toFloat y + 0.5 )
                        ]
                            ++ (possibleMovesPos ( x, y ) model.board
                                    |> List.map
                                        (\( x2, y2 ) ->
                                            G.circle 3
                                                |> G.outlined (G.solid 4) G.lightGreen
                                                |> drawPieceAt ( toFloat x2 + 0.5, toFloat y2 + 0.5 )
                                                |> G.notifyTap (ClickPiece (posToIndex ( x2, y2 )))
                                        )
                               )

                    Nothing ->
                        []
               )
        )


showCheckMate : Model -> List (G.Shape Msg)
showCheckMate { board, currentPlayer } =
    if currentPlayer /= None then
        [ Maybe.map (\pos ->
            let
                (s1, s2) = posToPosLabel pos
            in
                G.text ("Checkmate: " ++ s1 ++ "-" ++ s2)
        ) (checkPosIsMate currentPlayer board)
            |> Maybe.withDefault (G.text "")
            |> G.bold
            |> G.filled G.black
            |> G.move ( 0, -70 )
        ]
        else []


main : GraphicSVG.App.NotificationsApp Model Msg
main =
    GraphicSVG.App.notificationsApp
        { model = init
        , update = update
        , view = view
        }
