module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Maybe.Extra
import Random


type alias Area =
    Array (Array Pill)


initArea : Area
initArea =
    Array.repeat 16
        (Array.repeat
            8
            Empty
         -- (Array.repeat 4 (Virus Red)) (Array.repeat 4 (Virus Blue))
        )
        |> setToArea (Pill Red) ( 4, 0 )
        |> setToArea (Pill Red) ( 5, 0 )


randomPill : Random.Generator Pill
randomPill =
    Random.uniform Empty
        [ Virus Red

        -- , Virus Blue
        -- , Virus Yellow
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
    { count : Float, area : Area, activePill : Maybe ( Pos, Pos ) }


type Pill
    = Empty
    | Virus Colour
    | Pill Colour


type alias Pos =
    ( Int, Int )


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
                if cell == colour then
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
            area


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
                && y1_
                <= 15
                && y2_
                <= 15

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitArea area ->
            let
                a_ =
                    Debug.log "init" area
            in
            ( { model
                | area =
                    area
                        |> setToArea (Pill Red) ( 4, 0 )
                        |> setToArea (Pill Red) ( 5, 0 )
                , activePill = Just ( ( 5, 0 ), ( 4, 0 ) )
              }
            , Cmd.none
            )

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
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }


width =
    400


height =
    400


centerX =
    width / 2


centerY =
    height / 2


view : Model -> Html Msg
view { count, area } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            (clearScreen
                :: render (count / 25) area
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


render count area =
    let
        size =
            width / 3

        x =
            -(size / 2)

        y =
            -(size / 2)

        iCount =
            floor count

        -- currentC =
        --     if modBy 3 iCount == 0 then
        --         Blue
        --     else if modBy 3 iCount == 1 then
        --         Red
        --     else
        --         Yellow
        currentC =
            Blue
    in
    viewArea width height area



-- shapes
--     [ transform
--         [ translate centerX centerY
--         , rotate (degrees (count * 3))
--         ]
--     , fill <| colourToColor currentC
--     ]
--     [ rect ( x, y ) size size ]
