module Main exposing (main)

import Array
import Array.Extra
import Browser
import Dict
import Dict.Extra
import Html exposing (div, td, tr)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD


testJson =
    """
{
  "description": "Allows to look at json, search it etc.",
  "manifest_version": 2,
  "name": "Json Search",
  "version": "1.0",
  "homepage_url": "https://example.com",
  "icons": {
    "48": "icons/border-48.png"
  },
  "content_scripts": [
    {
      "matches": [
        "*://*.local/*"
      ],
      "js": [
        "elm.js",
        "json-search.js"
      ]
    }
  ]
}
"""


type alias Model =
    { node : Node
    , search : String
    , searchedNode : Maybe Node
    }


type alias Path =
    String


type alias Node =
    { value : JsonValue
    , path : Path
    }


type JsonValue
    = JString String
    | JInt Int
    | JArray (Array.Array ( Bool, Node ))
    | JDict (Dict.Dict String ( Bool, Node ))


isScalar : JsonValue -> Bool
isScalar json =
    case json of
        JInt _ ->
            True

        JString _ ->
            True

        _ ->
            False


addPathToNode : Path -> Node -> Node
addPathToNode parent node =
    case node.value of
        JString s ->
            { node | path = parent }

        JInt i ->
            { node | path = parent }

        JArray xs ->
            { value =
                JArray <|
                    Array.indexedMap
                        (\k ( b, v ) ->
                            let
                                path =
                                    (if parent == "" then
                                        ""

                                     else
                                        parent ++ "."
                                    )
                                        ++ String.fromInt k
                            in
                            ( b
                            , addPathToNode path v
                            )
                        )
                        xs
            , path = parent
            }

        JDict ds ->
            { value =
                JDict <|
                    Dict.map
                        (\k ( b, v ) ->
                            let
                                path =
                                    (if parent == "" then
                                        ""

                                     else
                                        parent ++ "."
                                    )
                                        ++ k
                            in
                            ( b
                            , addPathToNode path v
                            )
                        )
                        ds
            , path = parent
            }


addFalseToDict : Dict.Dict a b -> Dict.Dict a ( Bool, b )
addFalseToDict dict =
    Dict.map (\_ v -> ( False, v )) dict


addFalseToArray : Array.Array b -> Array.Array ( Bool, b )
addFalseToArray array =
    Array.map (\v -> ( False, v )) array


mkNode : JsonValue -> Node
mkNode value =
    { value = value
    , path = ""
    }


nodeDecoder : JD.Decoder Node
nodeDecoder =
    JD.oneOf
        [ JD.map (JString >> mkNode) JD.string
        , JD.map (JInt >> mkNode) JD.int
        , JD.map (addFalseToDict >> JDict >> mkNode) (JD.dict (JD.lazy (\_ -> nodeDecoder)))
        , JD.map (addFalseToArray >> JArray >> mkNode) (JD.array (JD.lazy (\_ -> nodeDecoder)))
        ]


viewNode : Node -> Html.Html Msg
viewNode json =
    Html.table []
        [ Html.tbody []
            (doViewNode json
                |> List.map (\( k, v ) -> Html.tr [] [ k, td [] [ v ] ])
            )
        ]


viewScalar : JsonValue -> Html.Html msg
viewScalar json =
    case json of
        JInt x ->
            Html.text (String.fromInt x)

        JString x ->
            Html.text x

        _ ->
            Html.text ""


viewToggle : Path -> Bool -> Html.Html Msg
viewToggle path open =
    Html.span
        ([ HA.style "background-image" "url('icons/arrow.svg')"
         , HA.style "background-position" "center"
         , HA.style "background-size" "10px"
         , HA.style "height" "14px"
         , HA.style "width" "14px"
         , HA.style "line-height" "14px"
         , HA.style "display" "inline-block"
         , HE.onClick (TogglePath path (not open))
         ]
            ++ (if open then
                    [ HA.style "transform" "rotate(-90deg)" ]

                else
                    []
               )
        )
        []


indentFromPath : Path -> Html.Attribute msg
indentFromPath path =
    let
        depth =
            String.split "." path |> List.length
    in
    HA.style "padding-inline-start" ((8 * depth |> String.fromInt) ++ "px")


doViewNode : Node -> List ( Html.Html Msg, Html.Html msg )
doViewNode json =
    case json.value of
        JString x ->
            [ ( div [] [], Html.text x ) ]

        JInt x ->
            [ ( div [] [], Html.text (String.fromInt x) ) ]

        JDict dict ->
            Dict.toList dict
                |> List.map
                    (\( k, ( open, v ) ) ->
                        let
                            key =
                                Html.text k
                        in
                        if isScalar v.value then
                            [ ( td [ indentFromPath v.path ] [ key ], viewScalar v.value ) ]

                        else
                            [ ( td [ indentFromPath v.path ] [ viewToggle v.path open, key ], Html.text "" ) ]
                                ++ (if open then
                                        doViewNode v

                                    else
                                        []
                                   )
                    )
                |> List.foldl List.append []

        JArray array ->
            Array.toIndexedList array
                |> List.map
                    (\( k, ( open, v ) ) ->
                        let
                            key =
                                Html.text (String.fromInt k)
                        in
                        if isScalar v.value then
                            [ ( td [ indentFromPath v.path ] [ key ], viewScalar v.value ) ]

                        else
                            [ ( td [ indentFromPath v.path ] [ viewToggle v.path open, key ], Html.text "" ) ]
                                ++ (if open then
                                        doViewNode v

                                    else
                                        []
                                   )
                    )
                |> List.foldl List.append []


type Msg
    = Noop
    | TogglePath Path Bool
    | SearchPath String
    | ExpandAll
    | CollapseAll


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        TogglePath path open ->
            { model
                | node =
                    updateOpenNode
                        (\path_ open_ ->
                            if path_ == path then
                                open

                            else
                                open_
                        )
                        model.node
                  , searchedNode = Maybe.map (\node -> updateOpenNode (\path_ open_ ->
                            if path_ == path then
                                open

                            else
                                open_
                        ) node) model.searchedNode
            }

        SearchPath search ->
            if String.length search > 0 then
                { model | search = search, searchedNode = searchNode search model.node }

            else
                { model | search = search, searchedNode = Nothing }

        ExpandAll ->
            { model | node = updateOpenNode (\_ _ -> True) model.node }

        CollapseAll ->
            { model | node = updateOpenNode (\_ _ -> False) model.node }


updateOpenNode : (Path -> Bool -> Bool) -> Node -> Node
updateOpenNode func node =
    case node.value of
        JArray xs ->
            { node
                | value =
                    JArray <|
                        Array.indexedMap
                            (\k ( b, v ) ->
                                ( func v.path b, updateOpenNode func v )
                            )
                            xs
            }

        JDict ds ->
            { node
                | value =
                    JDict <|
                        Dict.map
                            (\k ( b, v ) ->
                                ( func v.path b, updateOpenNode func v )
                            )
                            ds
            }

        _ ->
            node


searchNode : String -> Node -> Maybe Node
searchNode search node =
    case node.value of
        JString x ->
            if String.contains search x then
                Just node

            else
                Nothing

        JInt x ->
            if String.contains search (String.fromInt x) then
                Just node

            else
                Nothing

        JDict dict ->
            let
                res =
                    Dict.Extra.filterMap
                        (\k ( b, v ) ->
                            if String.contains search k then
                                Just (b, v)

                            else
                                searchNode search v
                                |> Maybe.map (\x -> (b, x))
                        )
                        dict
            in
            if Dict.size res > 0 then
                Just { node | value = JDict res }

            else
                Nothing

        JArray array ->
            let
                res =
                    Array.Extra.zip (List.range 0 (Array.length array) |> Array.fromList) array
                        |> Array.Extra.filterMap
                            (\( k, ( b, v ) ) ->
                                if String.contains search (String.fromInt k) then
                                    Just (b, v)

                                else
                                    searchNode search v
                                    |> Maybe.map (\x -> (b, x))
                            )
            in
            if Array.length res > 0 then
                Just { node | value = JArray res }

            else
                Nothing


viewBar : String -> Html.Html Msg
viewBar search =
    Html.div [ HA.style "display" "flex" ]
        [ Html.button [ HE.onClick ExpandAll ] [ Html.text "Expand all" ]
        , Html.button [ HE.onClick CollapseAll ] [ Html.text "Collapse all" ]
        , Html.span [ HA.style "flex-grow" "8" ]
            [ Html.input
                [ HA.type_ "textfield"
                , HA.value search
                , HE.onInput SearchPath
                ]
                []
            ]
        ]


view : Model -> Html.Html Msg
view { node, search, searchedNode } =
    div []
        [ viewBar search
        , Maybe.withDefault node searchedNode
        |> viewNode
        ]


main : Program () Model Msg
main =
    case JD.decodeString nodeDecoder testJson of
        Ok res ->
            Browser.sandbox
                { init =
                    { node = addPathToNode "" res, search = "", searchedNode = Nothing }
                , update = update
                , view = view
                }

        Err _ ->
            Browser.sandbox
                { init = { node = JString "" |> mkNode, search = "", searchedNode = Nothing }
                , update = \a b -> b
                , view = \x -> Html.text "error"
                }
