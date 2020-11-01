module Main exposing (main)

-- elm install avh4/elm-color

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes exposing (style)


view : Html msg
view =
    let
        width =
            500

        height =
            300
    in
    Canvas.toHtml ( width, height )
        [ style "border" "1px solid black" ]
        [ shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]
        , renderSquare
        ]


renderSquare =
    shapes [ fill (Color.rgba 0 0 0 1) ]
        [ rect ( 0, 0 ) 100 50 ]


main =
    view
