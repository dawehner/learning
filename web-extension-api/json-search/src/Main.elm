module Main exposing (main)

import Array
import Array.Extra
import Browser
import Dict
import Dict.Extra
import Html exposing (div, td, tr)
import Html.Attributes as HA exposing (class, style)
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


type Tab
    = TreeTab
    | RawTab


type alias Model =
    { tab : Tab
    , node : Node
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
                |> List.map (\( k, v ) -> Html.tr [ class "tree-element tree-row" ] [ k, td [] [ v ] ])
            )
        ]


viewScalar : JsonValue -> Html.Html msg
viewScalar json =
    case json of
        JInt x ->
            Html.span [ class "tree-number" ] [ Html.text (String.fromInt x) ]

        JString x ->
            Html.span [ class "tree-string" ] [ Html.text ("\"" ++ x ++ "\"") ]

        _ ->
            Html.text ""


viewClosedIndicator : JsonValue -> Html.Html msg
viewClosedIndicator json =
    case json of
        JArray _ ->
            Html.text "[…]"

        JDict _ ->
            Html.text "{…}"

        _ ->
            Html.text ""


viewToggle : Path -> Bool -> Html.Html Msg
viewToggle path open =
    Html.span
        ([ HA.style "background-image" "url('icons/arrow.svg')"
         , HA.style "background-position" "center"
         , HA.style "background-repeat" "no-repeat"
         , HA.style "background-size" "10px"
         , HA.style "height" "16px"
         , HA.style "width" "16px"
         , HA.style "line-height" "16px"
         , HA.style "display" "inline-block"
         , HA.style "cursor" "pointer"
         , HE.onClick (TogglePath path (not open))
         ]
            ++ (if open then
                    [ HA.style "transform" "rotate(-90deg)" ]

                else
                    []
               )
        )
        []


indentFromPath : Path -> Bool -> Html.Attribute msg
indentFromPath path hasToggle =
    let
        depth =
            (String.split "." path |> List.length) - 1
    in
    HA.style "padding-inline-start"
        ((16
            * (depth
                + (if hasToggle then
                    0

                   else
                    1
                  )
              )
            |> String.fromInt
         )
            ++ "px"
        )


doViewNode : Node -> List ( Html.Html Msg, Html.Html msg )
doViewNode json =
    case json.value of
        JString x ->
            [ ( div [] [], Html.text ("\"" ++ x ++ "\"") ) ]

        JInt x ->
            [ ( div [] [], Html.text (String.fromInt x) ) ]

        JDict dict ->
            Dict.toList dict
                |> List.map
                    (\( k, ( open, v ) ) ->
                        let
                            key =
                                Html.span [ class "tree-key" ] [ Html.text k ]
                        in
                        if isScalar v.value then
                            [ ( td [ indentFromPath v.path False ] [ key ], viewScalar v.value ) ]

                        else
                            [ ( td [ indentFromPath v.path True ] [ viewToggle v.path open, key ]
                              , if not open then
                                    viewClosedIndicator v.value

                                else
                                    Html.text ""
                              )
                            ]
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
                                Html.span [ class "tree-key" ] [ Html.text (String.fromInt k) ]
                        in
                        if isScalar v.value then
                            [ ( td [ indentFromPath v.path False ] [ key ], viewScalar v.value ) ]

                        else
                            [ ( td [ indentFromPath v.path True ] [ viewToggle v.path open, key ]
                              , if not open then
                                    viewClosedIndicator v.value

                                else
                                    Html.text ""
                              )
                            ]
                                ++ (if open then
                                        doViewNode v

                                    else
                                        []
                                   )
                    )
                |> List.foldl List.append []


prettyPrint : Int -> Node -> String
prettyPrint indent node =
    case node.value of
        JString s ->
            "\"" ++ s ++ "\","

        JInt int ->
            String.fromInt int ++ ","

        JArray xs ->
            let
                indent_ =
                    indent + 1
            in
            "[\n"
                ++ (Array.toIndexedList xs
                        |> List.map
                            (\( k, ( _, v ) ) ->
                                String.repeat indent_ "  "
                                    ++ "\""
                                    ++ String.fromInt k
                                    ++ "\": "
                                    ++ prettyPrint indent_ v
                            )
                        |> String.join "\n"
                   )
                ++ "\n"
                ++ String.repeat indent "  "
                ++ "],"

        JDict ds ->
            let
                indent_ =
                    indent + 1
            in
            "{\n"
                ++ (Dict.toList ds
                        |> List.map
                            (\( k, ( _, v ) ) ->
                                String.repeat indent_ "  "
                                    ++ "\""
                                    ++ k
                                    ++ "\": "
                                    ++ prettyPrint indent_ v
                            )
                        |> String.join "\n"
                   )
                ++ "\n"
                ++ String.repeat indent "  "
                ++ "},"


type Msg
    = Noop
    | TogglePath Path Bool
    | SearchPath String
    | ExpandAll
    | CollapseAll
    | ChooseTab Tab


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        ChooseTab tab ->
            { model | tab = tab }

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
                , searchedNode =
                    Maybe.map
                        (\node ->
                            updateOpenNode
                                (\path_ open_ ->
                                    if path_ == path then
                                        open

                                    else
                                        open_
                                )
                                node
                        )
                        model.searchedNode
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
                                Just ( b, v )

                            else
                                searchNode search v
                                    |> Maybe.map (\x -> ( b, x ))
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
                                    Just ( b, v )

                                else
                                    searchNode search v
                                        |> Maybe.map (\x -> ( b, x ))
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


viewTabBar : Tab -> Html.Html Msg
viewTabBar tab =
    Html.ul [ class "tab-bar" ]
        [ Html.li
            [ HE.onClick (ChooseTab TreeTab)
            , class
                (if tab == TreeTab then
                    "active"

                 else
                    ""
                )
            ]
            [ Html.text "Json" ]
        , Html.li
            [ HE.onClick (ChooseTab RawTab)
            , class
                (if tab == RawTab then
                    "active"

                 else
                    ""
                )
            ]
            [ Html.text "Raw" ]
        ]


view : Model -> Html.Html Msg
view { node, search, searchedNode, tab } =
    div []
        ([ viewTabBar tab
         ]
            ++ (case tab of
                    TreeTab ->
                        [ viewBar search
                        , Maybe.withDefault node searchedNode
                            |> viewNode
                        ]

                    RawTab ->
                        [ prettyPrint 0 node
                            |> Html.text
                            |> List.singleton
                            |> Html.pre []
                            |> List.singleton
                            |> Html.code []
                        ]
               )
        )


main : Program () Model Msg
main =
    case JD.decodeString nodeDecoder testJson of
        Ok res ->
            Browser.sandbox
                { init =
                    { node = addPathToNode "" res
                    , search = ""
                    , searchedNode = Nothing
                    , tab = TreeTab
                    }
                , update = update
                , view = view
                }

        Err _ ->
            Browser.sandbox
                { init =
                    { node = JString "" |> mkNode
                    , search = ""
                    , searchedNode = Nothing
                    , tab = TreeTab
                    }
                , update = \a b -> b
                , view = \x -> Html.text "error"
                }
