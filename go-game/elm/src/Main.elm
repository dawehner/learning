module Main exposing
    ( Colour(..)
    , GoGrid
    , Stone(..)
    , getAllOpenPoints
    , gridFromLists
    , hasOneOpenPoint
    , main
    )

import Array as A
import Browser
import Html exposing (Html)
import List.Extra
import Maybe.Extra
import Set


type alias Model =
    GoGrid


type alias GoGrid =
    A.Array (A.Array Stone)


gridFromLists : List (List Stone) -> GoGrid
gridFromLists xss =
    List.map A.fromList xss
        |> A.fromList


getPoint : ( Int, Int ) -> GoGrid -> Maybe Stone
getPoint ( x, y ) grid =
    A.get x grid
        |> Maybe.andThen
            (\row ->
                A.get y row
            )


isOpenPoint : ( Int, Int ) -> GoGrid -> Bool
isOpenPoint pos grid =
    getPoint pos grid
        |> Maybe.map ((==) NoStone)
        |> Maybe.withDefault False


gridHasAnyOpenPoint : GoGrid -> Bool
gridHasAnyOpenPoint grid =
    List.Extra.cartesianProduct
        [ List.range 0 (A.length grid - 1)
        , List.range 0 (A.length grid - 1)
        ]
        |> List.map
            (\pos ->
                case pos of
                    x1 :: x2 :: [] ->
                        Just ( x1, x2 )

                    _ ->
                        Nothing
            )
        |> Maybe.Extra.combine
        |> Maybe.map (List.any (\pos -> hasOneOpenPoint pos grid))
        |> Maybe.withDefault False


getAllOpenPoints : GoGrid -> List ( Int, Int )
getAllOpenPoints grid =
    List.Extra.cartesianProduct
        [ List.range 0 (A.length grid - 1)
        , List.range 0 (A.length grid - 1)
        ]
        |> List.map
            (\pos ->
                case pos of
                    x1 :: x2 :: [] ->
                        Just ( x1, x2 )

                    _ ->
                        Nothing
            )
        |> Maybe.Extra.combine
        |> Maybe.map (List.filter (\pos -> hasOneOpenPoint pos grid))
        |> Maybe.withDefault []


hasOneOpenPoint : ( Int, Int ) -> GoGrid -> Bool
hasOneOpenPoint ( x, y ) grid =
    List.any (\pos -> isOpenPoint pos grid)
        [ -- Above
          ( x, y + 1 )

        -- Below
        , ( x, y - 1 )

        -- Right
        , ( x + 1, y )

        -- Left
        , ( x - 1, y )
        ]


type Colour
    = Black
    | White


type Stone
    = Stone Colour
    | NoStone
    | Removed Colour


init =
    A.repeat 9 (A.repeat 9 NoStone)


type Msg
    = Noop


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model


view : Model -> Html Msg
view model =
    Html.pre [] [ Html.text <| viewGridString model ]


viewGridString : GoGrid -> String
viewGridString grid =
    A.foldl
        (\row string ->
            string
                ++ A.foldl
                    (\pos string_ ->
                        string_
                            ++ (case pos of
                                    Stone Black ->
                                        "X"

                                    Stone White ->
                                        "O"

                                    _ ->
                                        " "
                               )
                    )
                    ""
                    row
                ++ "\n"
        )
        ""
        grid


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
