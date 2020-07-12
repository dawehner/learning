module Main exposing (main)

import Array
import Dict
import Html
import Html.Attributes as HA
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


type alias Path =
    String


type alias Node =
    { value : JsonValue
    , keyPath : Path
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


addFalseToDict : Dict.Dict a b -> Dict.Dict a ( Bool, b )
addFalseToDict dict =
    Dict.map (\_ v -> ( False, v )) dict


addFalseToArray : Array.Array b -> Array.Array ( Bool, b )
addFalseToArray array =
    Array.map (\v -> ( False, v )) array


mkNode : JsonValue -> Node
mkNode value =
    { value = value
    , keyPath = ""
    }


jsonValueDecoder : JD.Decoder Node
jsonValueDecoder =
    JD.oneOf
        [ JD.map (JString >> mkNode) JD.string
        , JD.map (JInt >> mkNode) JD.int
        , JD.map (addFalseToDict >> JDict >> mkNode) (JD.dict (JD.lazy (\_ -> jsonValueDecoder)))
        , JD.map (addFalseToArray >> JArray >> mkNode) (JD.array (JD.lazy (\_ -> jsonValueDecoder)))
        ]


viewJsonValue : Node -> Html.Html msg
viewJsonValue json =
    Html.table []
        [ Html.tbody []
            (doViewJsonValue 1 json
                |> List.map (\( k, v ) -> Html.tr [] [ Html.td [] k, Html.td [] [ v ] ])
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


viewToggle : Bool -> Html.Html msg
viewToggle open =
    Html.span
        [ HA.style "background-image" "url('icons/arrow.svg')"
        , HA.style "background-position" "center"
        , HA.style "background-size" "10px"
        , HA.style "height" "14px"
        , HA.style "width" "14px"
        , HA.style "line-height" "14px"
        , HA.style "display" "inline-block"
        ]
        []


doViewJsonValue : Int -> Node -> List ( List (Html.Html msg), Html.Html msg )
doViewJsonValue depth json =
    case json.value of
        JString x ->
            [ ( [], Html.text x ) ]

        JInt x ->
            [ ( [], Html.text (String.fromInt x) ) ]

        JDict dict ->
            Dict.toList dict
                |> List.map
                    (\( k, ( open, v ) ) ->
                        let
                            key =
                                Html.text k
                        in
                        if isScalar v.value then
                            [ ( [ key ], viewScalar v.value ) ]

                        else
                            [ ( [ viewToggle open, key ], Html.text "" ) ]
                                ++ doViewJsonValue (depth + 1) v
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
                            [ ( [ key ], viewScalar v.value ) ]

                        else
                            [ ( [ viewToggle open, key ], Html.text "" ) ]
                                ++ doViewJsonValue (depth + 1) v
                    )
                |> List.foldl List.append []


main =
    case JD.decodeString jsonValueDecoder testJson of
        Ok res ->
            viewJsonValue res

        Err _ ->
            Html.text "error"
