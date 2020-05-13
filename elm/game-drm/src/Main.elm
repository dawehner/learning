module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyUp)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Maybe.Extra
import Random


type alias Area =
    Array (Array Pill)


type KeyboardDirection
    = MoveLeft
    | MoveRight
    | MoveDown
    | FlipLeft
    | FlipRight
    | Other


keyDecoder : JD.Decoder Msg
keyDecoder =
    JD.map
        (\string ->
            KeyboardEvent
                (case string of
                    "ArrowLeft" ->
                        MoveLeft

                    "ArrowRight" ->
                        MoveRight

                    "ArrowDown" ->
                        MoveDown

                    _ ->
                        Other
                )
        )
        (JD.field "key" JD.string)


initArea : Area
initArea =
    emptyArea
        |> setToArea (Pill Red) ( 4, 0 )
        |> setToArea (Pill Red) ( 5, 0 )


randomPill : Random.Generator Pill
randomPill =
    Random.uniform Empty
        [ Virus Red

        -- , Virus Blue
        -- , Virus Yellow
        ]


randomColour : Random.Generator Colour
randomColour =
    Random.uniform Red
        [ Blue
        , Yellow
        ]


randomArea : Random.Generator Area
randomArea =
    let
        maxHeight =
            8

        randomRow =
            Random.map Array.fromList (Random.list 8 randomPill)

        randomRows =
            Random.map Array.fromList (Random.list maxHeight randomRow)
    in
    Random.map
        (\rows ->
            Array.append (Array.repeat 8 (Array.repeat 8 Empty)) rows
        )
        randomRows



-- max


type alias Model =
    { count : Float
    , area : Area
    , activePill : Maybe ( Pos, Pos )
    }


type Pill
    = Empty
    | Virus Colour
    | Pill Colour


type alias Pos =
    ( Int, Int )


emptyArea : Area
emptyArea =
    Array.repeat 16
        (Array.repeat
            8
            Empty
        )


removeFromArea : Pos -> Area -> Area
removeFromArea =
    setToArea Empty


removeXRangeFromArea : Pos -> Int -> Area -> Area
removeXRangeFromArea ( x, y ) n area =
    Array.Extra.update y
        (\row ->
            List.range 0 n
                |> List.foldl
                    (\n_ row_ ->
                        Array.set (x + n_) Empty row_
                    )
                    row
        )
        area


removeYRangeFromArea : Pos -> Int -> Area -> Area
removeYRangeFromArea ( x, y ) n area =
    let
        range =
            List.range 0 n
    in
    List.foldl
        (\n_ area_ -> setToArea Empty ( x, y + n_ ) area_)
        area
        range


setToArea : Pill -> Pos -> Area -> Area
setToArea a ( x, y ) area =
    Array.Extra.update y
        (Array.set x a)
        area


hasInArea : Pos -> Area -> Bool
hasInArea ( x, y ) area =
    Array.get y area
        |> Maybe.andThen (Array.get x)
        |> (/=) (Just Empty)


getFromArea : Pos -> Area -> Maybe Pill
getFromArea ( x, y ) area =
    Array.get y area
        |> Maybe.andThen (Array.get x)


pos4InArray : Array Pill -> Maybe ( Int, Int )
pos4InArray array =
    Array.map pillToColur array
        |> Array.foldl
            (\cell ( colour, ( count, currentPos ), pos ) ->
                if cell /= None && cell == colour then
                    if count + 1 >= 4 then
                        ( cell, ( count + 1, currentPos + 1 ), Just ( currentPos - count, count + 1 ) )

                    else
                        ( cell, ( count + 1, currentPos + 1 ), pos )

                else
                    ( cell, ( 1, currentPos + 1 ), pos )
            )
            ( None, ( 0, 0 ), Nothing )
        |> (\( _, _, res ) -> res)


pos4InRow : Int -> Area -> Maybe ( Pos, Int )
pos4InRow y area =
    Array.get y area
        |> Maybe.andThen pos4InArray
        |> Maybe.map (\( x, count ) -> ( ( x, y ), count ))


pos4InColumn : Int -> Area -> Maybe ( Pos, Int )
pos4InColumn x area =
    Array.map
        (Array.get x)
        area
        |> Maybe.Extra.combineArray
        |> Maybe.andThen pos4InArray
        |> Maybe.map (\( y, count ) -> ( ( x, y ), count ))


checkAreaFor4s : Area -> Area
checkAreaFor4s area =
    let
        filterRow area0 =
            List.range 0 15
                |> List.foldl
                    (\y area_ ->
                        let
                            result =
                                pos4InRow y area_
                        in
                        if result == Nothing then
                            area_

                        else
                            Maybe.map
                                (\( pos, count ) ->
                                    removeXRangeFromArea pos count area_
                                )
                                result
                                |> Maybe.withDefault area_
                    )
                    area0

        filterColumn area0 =
            List.range 0 8
                |> List.foldl
                    (\x area_ ->
                        let
                            result =
                                pos4InColumn x area_
                        in
                        if result == Nothing then
                            area_

                        else
                            Maybe.map
                                (\( pos, count ) ->
                                    removeYRangeFromArea pos count area_
                                )
                                result
                                |> Maybe.withDefault area_
                    )
                    area0
    in
    filterRow area
        |> filterColumn


movePillDown : Pos -> Pos -> Area -> ( Maybe ( Pos, Pos ), Area )
movePillDown ( x1, y1 ) ( x2, y2 ) area =
    -- @todo figure out collisions
    let
        y1_ =
            y1
                + 1

        y2_ =
            y2 + 1

        canMove =
            not (hasInArea ( x1, y1_ ) area)
                && not (hasInArea ( x2, y2_ ) area)
                && (y1_
                        <= 15
                   )
                && (y2_
                        <= 15
                   )

        mel1 =
            getFromArea ( x1, y1 ) area

        mel2 =
            getFromArea ( x2, y2 ) area
    in
    if canMove then
        ( Just ( ( x1, y1_ ), ( x2, y2_ ) )
        , Maybe.map2
            (\el1 el2 ->
                removeFromArea ( x1, y1 ) area
                    |> removeFromArea ( x2, y2 )
                    |> setToArea el1 ( x1, y1_ )
                    |> setToArea el2 ( x2, y2_ )
                    |> checkAreaFor4s
            )
            mel1
            mel2
            |> Maybe.withDefault area
        )

    else
        ( Nothing, area )


pillToColur : Pill -> Colour
pillToColur pill =
    case pill of
        Empty ->
            None

        Virus c ->
            c

        Pill c ->
            c


type Colour
    = Blue
    | Red
    | Yellow
    | None


type Msg
    = Frame Float
    | InitArea Area
    | NewActivePills ( Colour, Colour )
    | KeyboardEvent KeyboardDirection


generateNewActivePills : Cmd Msg
generateNewActivePills =
    Random.generate NewActivePills (Random.map2 (\x y -> ( x, y )) randomColour randomColour)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitArea area ->
            ( { model
                | area =
                    area
                        |> setToArea (Pill Red) ( 4, 0 )
                        |> setToArea (Pill Red) ( 5, 0 )
                , activePill = Just ( ( 5, 0 ), ( 4, 0 ) )
              }
            , Cmd.none
            )

        NewActivePills ( c1, c2 ) ->
            ( { model
                | area =
                    model.area
                        |> setToArea (Pill c1) ( 4, 0 )
                        |> setToArea (Pill c2) ( 5, 0 )
                , activePill = Just ( ( 5, 0 ), ( 4, 0 ) )
              }
            , Cmd.none
            )

        KeyboardEvent key ->
            case key of
                MoveLeft ->
                    ( Maybe.map
                        (\( pos1, pos2 ) ->
                            let
                                hitLeftBorder =
                                    Tuple.first pos1 <= 0 || Tuple.first pos2 <= 0

                                pos1_ =
                                    if hitLeftBorder then
                                        pos1

                                    else
                                        Tuple.mapFirst (\x -> max (x - 1) 0) pos1

                                pos2_ =
                                    if hitLeftBorder then
                                        pos2

                                    else
                                        Tuple.mapFirst (\x -> max (x - 1) 0) pos2
                            in
                            Maybe.map2
                                (\el1 el2 ->
                                    { model
                                        | activePill =
                                            Just
                                                ( pos1_
                                                , pos2_
                                                )
                                        , area =
                                            removeFromArea pos1 model.area
                                                |> removeFromArea pos2
                                                |> setToArea el1 pos1_
                                                |> setToArea el2 pos2_
                                    }
                                )
                                (getFromArea pos1 model.area)
                                (getFromArea pos2 model.area)
                                |> Maybe.withDefault model
                        )
                        model.activePill
                        |> Maybe.withDefault model
                    , Cmd.none
                    )

                MoveRight ->
                    ( Maybe.map
                        (\( pos1, pos2 ) ->
                            let
                                hitRightBorder =
                                    Tuple.first pos1 >= 7 || Tuple.first pos2 >= 7

                                pos1_ =
                                    if hitRightBorder then
                                        pos1

                                    else
                                        Tuple.mapFirst (\x -> min (x + 1) 7) pos1

                                pos2_ =
                                    if hitRightBorder then
                                        pos2

                                    else
                                        Tuple.mapFirst (\x -> min (x + 1) 7) pos2
                            in
                            Maybe.map2
                                (\el1 el2 ->
                                    { model
                                        | activePill =
                                            Just
                                                ( pos1_
                                                , pos2_
                                                )
                                        , area =
                                            removeFromArea pos1 model.area
                                                |> removeFromArea pos2
                                                |> setToArea el1 pos1_
                                                |> setToArea el2 pos2_
                                    }
                                )
                                (getFromArea pos1 model.area)
                                (getFromArea pos2 model.area)
                                |> Maybe.withDefault model
                        )
                        model.activePill
                        |> Maybe.withDefault model
                    , Cmd.none
                    )

                MoveDown ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Frame _ ->
            if modBy 50 (floor model.count) == 0 then
                let
                    ( activePill_, area_ ) =
                        Maybe.map
                            (\( pos1, pos2 ) ->
                                movePillDown pos1 pos2 model.area
                            )
                            model.activePill
                            |> Maybe.withDefault ( model.activePill, model.area )
                in
                if activePill_ == Nothing then
                    ( { model
                        | count = model.count + 1
                        , activePill = Nothing
                        , area = area_
                      }
                    , generateNewActivePills
                    )

                else
                    ( { model
                        | count = model.count + 1
                        , activePill = activePill_
                        , area = area_
                      }
                    , Cmd.none
                    )

            else
                ( { model
                    | count = model.count + 1
                  }
                , Cmd.none
                )


main : Program () Model Msg
main =
    Browser.element
        { init =
            \() ->
                ( { count = 0
                  , area = initArea
                  , activePill = Nothing
                  }
                , Random.generate InitArea randomArea
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch [ onAnimationFrameDelta Frame, onKeyUp keyDecoder ]
        }


width : number
width =
    400


height : number
height =
    400


centerX : Float
centerX =
    width / 2


centerY : Float
centerY =
    height / 2


view : Model -> Html Msg
view { area } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            (clearScreen
                :: render area
            )
        ]


clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


colourToColor : Colour -> Color.Color
colourToColor colour =
    case colour of
        Blue ->
            Color.blue

        Red ->
            Color.red

        Yellow ->
            Color.yellow

        None ->
            Color.white


flattenArray : Array (Array a) -> Array a
flattenArray =
    Array.foldl Array.append Array.empty


viewArea : Float -> Float -> Area -> List Renderable
viewArea h w area =
    let
        ySize =
            h / toFloat (Array.length area)
    in
    Array.indexedMap
        (\indexY row ->
            let
                xSize =
                    w / toFloat (Array.length row)
            in
            Array.indexedMap
                (\indexX cell ->
                    rect ( toFloat indexX * xSize, toFloat indexY * ySize ) (xSize - 10.0) (ySize - 10.0)
                        |> List.singleton
                        |> shapes [ fill (pillToColur cell |> colourToColor) ]
                )
                row
        )
        area
        |> flattenArray
        |> Array.toList


render area =
    viewArea width height area



-- shapes
--     [ transform
--         [ translate centerX centerY
--         , rotate (degrees (count * 3))
--         ]
--     , fill <| colourToColor currentC
--     ]
--     [ rect ( x, y ) size size ]
