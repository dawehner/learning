module Main exposing (main)

import Html
import Json.Decode as JD
import Array
import Dict

testJson = """
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

type JsonValue = JString String
  | JInt Int
  | JArray (Array.Array JsonValue)
  | JDict (Dict.Dict String JsonValue)

isScalar : JsonValue -> Bool
isScalar json =
  case json of
    JInt _ -> True
    JString _-> True
    _ -> False

jsonValueDecoder : JD.Decoder JsonValue
jsonValueDecoder = 
  JD.oneOf 
    [
      (JD.map JString JD.string),
      (JD.map JInt JD.int),
      (JD.map JDict (JD.dict (JD.lazy (\_ -> jsonValueDecoder)))),
      (JD.map JArray (JD.array (JD.lazy (\_ -> jsonValueDecoder))))
    ]

viewJsonValue : JsonValue -> Html.Html msg
viewJsonValue json =
  Html.table [] [
    Html.tbody [] (doViewJsonValue 1 json
    |> List.map (\(k, v) -> Html.tr [] [Html.td [] [k], Html.td [] [v]])
    )
  ]

viewScalar : JsonValue -> Html.Html msg
viewScalar json =
  case json of
    JInt x -> Html.text (String.fromInt x)
    JString x -> Html.text x
    _ -> Html.text ""

doViewJsonValue : Int -> JsonValue -> List (Html.Html msg, Html.Html msg)
doViewJsonValue depth json = 
  case json of
    JString x -> [(Html.text "string", Html.text x)]
    JInt x -> [(Html.text "int", Html.text (String.fromInt x))]
    JDict dict -> 
      Dict.toList dict
      |> List.map (\(k, v) -> 
        let
          key = Html.text (String.repeat depth " " ++ k)
        in
          if isScalar v then
            [(key, viewScalar v)]
          else
            [(key, Html.text "")]
            ++ (doViewJsonValue (depth + 1) v))
      |> List.foldl List.append []
      
    JArray array -> 
      Array.toIndexedList array
      |> List.map (\(k, v) -> 
        let
          key = Html.text (String.repeat depth " " ++ (String.fromInt k))
        in
          if isScalar v then
            [(key, viewScalar v)]
          else
            [(key, Html.text "")]
            ++ (doViewJsonValue (depth + 1) v))
      |> List.foldl List.append []
      

main = 
  case JD.decodeString jsonValueDecoder testJson of
    Ok res -> viewJsonValue res
    Err _ -> Html.text "error"