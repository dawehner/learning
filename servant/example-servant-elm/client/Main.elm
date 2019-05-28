module Main exposing (..)

import Api exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task exposing (Task, perform)
import Model exposing (..)


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



init : ( Model, Cmd Msg )
init =
    let
        fetch =
            toServer Initial Api.getApiItem

        state =
            { items = empty, addItemInput = "", error = Nothing }
    in
    ( state, fetch )



-- UPDATE



update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        FromServer fromServer ->
            case fromServer of
                Initial itemIds ->
                    let
                        cmd : Cmd Msg
                        cmd =
                            Cmd.batch <|
                                List.map (toServer NewItem << getApiItemByItemId) itemIds
                    in
                    ( s, cmd )

                NewItem item ->
                    { s | items = insert item.id item s.items } ! []

                Delete id ->
                    { s | items = remove id s.items } ! []

        FromUi fromUi ->
            case fromUi of
                AddItemButton ->
                    let
                        new =
                            s.addItemInput

                        cmd =
                            toServer (\id -> NewItem (Item id new)) (postApiItem new)

                        newState =
                            { s | addItemInput = "" }
                    in
                    if new == "" then
                        update (Error "empty field") s
                    else
                        ( newState, cmd )

                AddItemInputChange t ->
                    { s | addItemInput = t } ! []

                Done id ->
                    let
                        cmd =
                            toServer (always (Delete id)) (deleteApiItemByItemId id)
                    in
                    ( s, cmd )

        Error msg ->
            ( { s | error = Just msg }, Cmd.none )


-- VIEW


view : Model -> Html Msg
view state =
    div [] <|
        [ text (toString state)
        , br [] []
        ]
            ++ List.map (viewItem << snd) (toList state.items)
            ++ [ input [ onInput (FromUi << AddItemInputChange) ] []
               , button [ onClick (FromUi AddItemButton) ] [ text "add item" ]
               ]


viewItem : Item -> Html Msg
viewItem item =
    div [] <|
        [ text item.text
        , text " - "
        , button [ onClick (FromUi <| Done item.id) ] [ text "done" ]
        ]
