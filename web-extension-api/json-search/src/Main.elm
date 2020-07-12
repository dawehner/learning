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

addPathToNode : KeyPath -> Node -> Node
addPathToNode parent node =
  case node.value of
    JString s -> node
    JInt i -> node
    JArray xs -> Array.indexedMap (\k v -> 
      let keyPath = parent ++ '.' ++ (String.fromInt k)
      { v | keyPath = keyPath
      , value = addPathToNode keyPath v
      }
      )
    JDict ds -> Dict.map (\k v -> 
      let keyPath = parent ++ '.' ++ k
      { v | keyPath = keyPath
      , value = addPathToNode keyPath v
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
    , keyPath = ""
    }


nodeDecoder : JD.Decoder Node
nodeDecoder =
    JD.oneOf
        [ JD.map (JString >> mkNode) JD.string
        , JD.map (JInt >> mkNode) JD.int
        , JD.map (addFalseToDict >> JDict >> mkNode) (JD.dict (JD.lazy (\_ -> nodeDecoder)))
        , JD.map (addFalseToArray >> JArray >> mkNode) (JD.array (JD.lazy (\_ -> nodeDecoder)))
        ]


viewNode : Node -> Html.Html msg
viewNode json =
    Html.table []
        [ Html.tbody []
            (doViewNode 1 json
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


doViewNode : Int -> Node -> List ( List (Html.Html msg), Html.Html msg )
doViewNode depth json =
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
                                ++ doViewNode (depth + 1) v
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
                                ++ doViewNode (depth + 1) v
                    )
                |> List.foldl List.append []


main =
    case JD.decodeString nodeDecoder testJson of
        Ok res ->
            viewNode res

        Err _ ->
            Html.text "error"
