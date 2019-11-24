module Main exposing (..)

import Browser
import Color
import Dict
import Element as E
import Element.Background as EBackground
import Element.Events as EEvents
import Element.Region as ERegion
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import List.Extra
import List.Selection as LS
import Maybe.Extra
import Random
import Vector1
import Vector2
import Vector3
import Vector4
import Vector5



---- MODEL ----


type alias Model =
    { piles : Dict.Dict PileId Pile
    , storeField : Dict.Dict Player StoreField
    , currentField : Dict.Dict Player CurrentField
    , players : List Player
    , currentPlayer : Player
    }


type alias Player =
    String


playerFromString : String -> Player
playerFromString =
    identity


type TokenType
    = TokenOne
    | TokenTwo
    | TokenThree
    | TokenFour
    | TokenFive


type alias Pile =
    Vector4.Vector4 (Maybe TokenType)


type alias PileId =
    Int


type alias Piles =
    Dict.Dict Int Pile


isVec4Empty : Vector4.Vector4 (Maybe a) -> Bool
isVec4Empty =
    Vector4.toList >> List.all ((==) Nothing)


type alias CurrentField =
    Vector5.Vector5 (Vector5.Vector5 (Maybe TokenType))


emptyCurrentField : CurrentField
emptyCurrentField =
    Vector5.from5
        (Vector5.from5 Nothing Nothing Nothing Nothing Nothing)
        (Vector5.from5 Nothing Nothing Nothing Nothing Nothing)
        (Vector5.from5 Nothing Nothing Nothing Nothing Nothing)
        (Vector5.from5 Nothing Nothing Nothing Nothing Nothing)
        (Vector5.from5 Nothing Nothing Nothing Nothing Nothing)


type alias StoreField =
    { store1 : Vector1.Vector1 (Maybe TokenType)
    , store2 : Vector2.Vector2 (Maybe TokenType)
    , store3 : Vector3.Vector3 (Maybe TokenType)
    , store4 : Vector4.Vector4 (Maybe TokenType)
    , store5 : Vector5.Vector5 (Maybe TokenType)
    }


emptyStoreField : StoreField
emptyStoreField =
    { store1 = Vector1.from1 Nothing
    , store2 = Vector2.from2 Nothing Nothing
    , store3 = Vector3.from3 Nothing Nothing Nothing
    , store4 = Vector4.from4 Nothing Nothing Nothing Nothing
    , store5 = Vector5.from5 Nothing Nothing Nothing Nothing Nothing
    }


storeFieldToList : StoreField -> List (List (Maybe TokenType))
storeFieldToList { store1, store2, store3, store4, store5 } =
    [ Vector1.toList store1
    , Vector2.toList store2
    , Vector2.toList store2
    , Vector3.toList store3
    , Vector4.toList store4
    , Vector5.toList store5
    ]


generateCurrentField : Random.Generator CurrentField
generateCurrentField =
    let
        singleToken =
            Random.weighted ( 25, Nothing )
                [ ( 15, Just TokenOne )
                , ( 15, Just TokenTwo )
                , ( 15, Just TokenThree )
                , ( 15, Just TokenFour )
                , ( 15, Just TokenFive )
                ]
    in
    Random.list 25 singleToken
        |> Random.map
            (\tokens ->
                List.Extra.groupsOf 5 tokens
                    |> List.map (\tokens_ -> Vector5.fromList tokens_ |> Maybe.map Tuple.second)
                    |> Maybe.Extra.combine
                    |> Maybe.andThen
                        (\tokenRows -> Vector5.fromList tokenRows |> Maybe.map Tuple.second)
                    |> Maybe.withDefault emptyCurrentField
            )


generateVector1 : Random.Generator a -> Random.Generator (Vector1.Vector1 a)
generateVector1 generator =
    Random.map Vector1.from1 generator


generateVector2 : Random.Generator a -> Random.Generator (Vector2.Vector2 a)
generateVector2 generator =
    Random.map2 Vector2.from2 generator generator


generateVector3 : Random.Generator a -> Random.Generator (Vector3.Vector3 a)
generateVector3 generator =
    Random.map3 Vector3.from3 generator generator generator


generateVector4 : Random.Generator a -> Random.Generator (Vector4.Vector4 a)
generateVector4 generator =
    Random.map4 Vector4.from4 generator generator generator generator


generateVector5 : Random.Generator a -> Random.Generator (Vector5.Vector5 a)
generateVector5 generator =
    Random.map5 Vector5.from5 generator generator generator generator generator


generateStoreField : Random.Generator StoreField
generateStoreField =
    let
        singleToken =
            Random.weighted ( 35, Nothing )
                [ ( 10, Just TokenOne )
                , ( 10, Just TokenTwo )
                , ( 10, Just TokenThree )
                , ( 10, Just TokenFour )
                , ( 10, Just TokenFive )
                ]
    in
    Random.map5
        (\store1 store2 store3 store4 store5 ->
            { store1 = store1, store2 = store2, store3 = store3, store4 = store4, store5 = store5 }
        )
        (generateVector1 singleToken)
        (generateVector2 singleToken)
        (generateVector3 singleToken)
        (generateVector4 singleToken)
        (generateVector5 singleToken)


generatePiles : Random.Generator Piles
generatePiles =
    Random.list 5 generatePile
        |> Random.map (List.Extra.zip (List.range 0 5))
        |> Random.map Dict.fromList


generatePile : Random.Generator Pile
generatePile =
    let
        singleToken =
            Random.uniform TokenOne [ TokenTwo, TokenThree, TokenFive, TokenFive ]
    in
    Random.map4 (\x1 x2 x3 x4 -> Vector4.from4 (Just x1) (Just x2) (Just x3) (Just x4))
        singleToken
        singleToken
        singleToken
        singleToken


init : ( Model, Cmd Msg )
init =
    ( { currentField =
            Dict.empty
                |> Dict.insert "1" emptyCurrentField
                |> Dict.insert "2" emptyCurrentField
      , storeField =
            Dict.empty
                |> Dict.insert "1" emptyStoreField
                |> Dict.insert "2" emptyStoreField
      , piles = Dict.empty
      , currentPlayer = "1"
      , players = [ "First", "Second" ]
      }
    , Cmd.batch
        [ --      Random.generate (CurrentFieldGenerated "1") generateCurrentField
          -- , Random.generate (CurrentFieldGenerated "2") generateCurrentField
          -- , Random.generate (StoreFieldGenerated "1") generateStoreField
          -- , Random.generate (StoreFieldGenerated "2") generateStoreField
          Random.generate PilesGenerated generatePiles
        ]
    )



---- UPDATE ----


type Msg
    = NoOp
    | CurrentFieldGenerated Player CurrentField
    | StoreFieldGenerated Player StoreField
    | PilesGenerated Piles
    | Draw Player PileId TokenType


drawFromPile : PileId -> TokenType -> Piles -> ( List TokenType, Piles )
drawFromPile pileId tokenType piles =
    let
        result =
            Dict.get pileId piles
                |> Maybe.map (\pile -> removeTokensFromPile tokenType pile)
    in
    case result of
        Nothing ->
            ( [], piles )

        Just ( tokens, pile ) ->
            ( tokens, Dict.insert pileId pile piles )


separateList : (a -> Bool) -> List a -> ( List a, List a )
separateList func xs =
    List.foldl
        (\x agg ->
            if func x then
                Tuple.mapFirst ((::) x) agg

            else
                Tuple.mapSecond ((::) x) agg
        )
        ( [], [] )
        xs


removeTokensFromPile : TokenType -> Pile -> ( List TokenType, Pile )
removeTokensFromPile tokenType pile =
    Vector4.toList pile
        |> separateList ((==) (Just tokenType))
        |> (\( maybeTokens, remaining ) ->
                ( Maybe.Extra.values maybeTokens
                , remaining
                    |> Vector4.fromListWithDefault Nothing
                    |> Tuple.second
                )
           )


fill5WithNothing : List (Maybe a) -> Vector5.Vector5 (Maybe a)
fill5WithNothing xs =
    let
        missing =
            5 - List.length xs
    in
    (if missing == 0 then
        xs

     else if missing > 0 then
        List.repeat missing Nothing ++ xs

     else
        List.drop (abs missing) xs
    )
        |> Vector5.fromList
        |> Maybe.map Tuple.second
        |> Maybe.withDefault (Vector5.from5 Nothing Nothing Nothing Nothing Nothing)


addTokenToRow : List TokenType -> Vector5.Vector5 (Maybe TokenType) -> Vector5.Vector5 (Maybe TokenType)
addTokenToRow tokens row =
    Vector5.toList row
        |> Maybe.Extra.combine
        |> Maybe.map (List.append tokens)
        |> Maybe.map (List.map Just)
        |> Maybe.withDefault (List.map Just tokens)
        |> fill5WithNothing


addTokensToCurrentField : List TokenType -> CurrentField -> CurrentField
addTokensToCurrentField tokens currentField =
    case tokens of
        [] ->
            currentField

        t :: ts ->
            Vector5.toIndexedList currentField
                |> List.filter (\( index, row ) -> Vector5.toList row |> List.any ((==) (Just t)))
                |> List.head
                |> Maybe.map (\( index, row ) -> ( index, addTokenToRow tokens row ))
                |> Maybe.map (\( index, row ) -> Vector5.set index row currentField)
                |> Maybe.withDefault currentField



-- @TODO Implement overflow piles
-- Vector5.mapItem index (\)
-- Vector5.set
-- Debug.todo "meh"


updateActivePlayer : Model -> Model
updateActivePlayer model =
    let
        ( nextPlayer, nextPlayers ) =
            Maybe.map2 (\player restPlayers -> ( player, restPlayers ++ [ model.currentPlayer ] ))
                (List.head model.players)
                (List.tail model.players)
                |> Maybe.withDefault ( model.currentPlayer, model.players )
    in
    { model
        | currentPlayer = nextPlayer
        , players = nextPlayers
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CurrentFieldGenerated playerId field ->
            ( { model | currentField = Dict.insert playerId field model.currentField }, Cmd.none )

        StoreFieldGenerated playerId field ->
            ( { model | storeField = Dict.insert playerId field model.storeField }, Cmd.none )

        PilesGenerated piles ->
            ( { model | piles = Debug.log "piles" piles }, Cmd.none )

        Draw player pileId tokenType ->
            let
                ( tokens, piles ) =
                    drawFromPile pileId tokenType model.piles
            in
            ( { model
                | piles = piles
                , currentField =
                    Dict.update player
                        (Maybe.map
                            (addTokensToCurrentField tokens)
                        )
                        model.currentField
              }
                |> updateActivePlayer
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    E.layout [] <|
        E.column []
            [ E.el
                [ ERegion.heading 1 ]
                (E.text "Piles")
            , viewPiles model.currentPlayer model.piles
            , E.el [ ERegion.heading 1 ] (E.text "currentField")
            , Dict.toList model.currentField
                |> List.map
                    (\( player, currentField ) ->
                        E.column []
                            [ E.el [ ERegion.heading 2 ] (E.text ("player: " ++ player))
                            , viewCurrentField currentField
                            ]
                    )
                |> E.column []

            -- , E.el [ ERegion.heading 1 ] (E.text "storeField")
            -- , viewStoreField model.storeField
            ]


viewEmptyToken : E.Element Msg
viewEmptyToken =
    E.el
        [ E.width (E.px 60)
        , E.height (E.px 60)
        ]
        E.none


viewCurrentField : CurrentField -> E.Element Msg
viewCurrentField field =
    Vector5.toList field
        |> List.map
            (\tokenRow ->
                Vector5.toList tokenRow
                    |> List.map
                        (\token ->
                            Maybe.map (viewToken Nothing) token
                                |> Maybe.withDefault
                                    viewEmptyToken
                        )
                    |> E.row [ E.spacing 5 ]
            )
        |> E.column [ E.spacing 5 ]


viewStoreField : StoreField -> E.Element Msg
viewStoreField storeField =
    storeFieldToList storeField
        |> List.map
            (\tokenRow ->
                List.map
                    (\token ->
                        Maybe.map (viewToken Nothing) token
                            |> Maybe.withDefault viewEmptyToken
                    )
                    tokenRow
                    |> E.row [ E.spacing 5 ]
            )
        |> E.column [ E.spacing 5 ]


viewToken : Maybe Msg -> TokenType -> E.Element Msg
viewToken onClick tokenType =
    E.el
        ([ E.width (E.px 60)
         , E.height (E.px 60)
         , EEvents.onClick (Maybe.withDefault NoOp onClick)
         , EBackground.color <|
            (\{ red, green, blue, alpha } -> E.rgba red green blue alpha) <|
                Color.toRgba <|
                    case tokenType of
                        TokenOne ->
                            Color.red

                        TokenTwo ->
                            Color.blue

                        TokenThree ->
                            Color.green

                        TokenFour ->
                            Color.orange

                        TokenFive ->
                            Color.black
         ]
            ++ (if onClick == Nothing then
                    []

                else
                    [ E.pointer ]
               )
        )
        E.none


viewPile : Player -> PileId -> Pile -> E.Element Msg
viewPile player pileId pile =
    let
        viewPileToken =
            \maybeToken ->
                Maybe.map
                    (\token ->
                        viewToken
                            (Draw player pileId token |> Just)
                            token
                    )
                    maybeToken
                    |> Maybe.withDefault viewEmptyToken
    in
    E.column [ E.spacing 5 ]
        [ E.row [ E.spacing 5 ]
            [ Vector4.get Vector4.Index0 pile |> viewPileToken
            , Vector4.get Vector4.Index1 pile |> viewPileToken
            ]
        , E.row [ E.spacing 5 ]
            [ Vector4.get Vector4.Index2 pile |> viewPileToken
            , Vector4.get Vector4.Index3 pile |> viewPileToken
            ]
        ]


viewPiles : Player -> Piles -> E.Element Msg
viewPiles player piles =
    List.map (\( index, pile ) -> viewPile player index pile) (Dict.toList piles)
        |> E.row [ E.spacing 15 ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
