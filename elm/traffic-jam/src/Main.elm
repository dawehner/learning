module Main exposing (main)

import Dict
import Html


type alias Car =
    { speed : Int
    , pos : Int
    }


type alias Cars =
    Dict.Dict Int Car


moveCar : Car -> Dict.Dict Int Car -> Dict.Dict Int Car
moveCar car cars =
    let
        nextPos =
            modBy (car.pos + car.speed) 50
    in
    Dict.update nextPos
        (\mCar ->
            case mCar of
                Nothing ->
                    Just
                        { speed = car.speed
                        , pos = nextPos
                        }

                _ ->
                    Nothing
        )
        cars
    |> 


moveCars : Cars -> Cars
moveCars cars =
    Dict.foldl (\_ car cars_ -> moveCar car cars_) cars cars


showCars : Cars -> String
showCars cars =
    let
        keys =
            Dict.keys cars

        maxKey =
            List.foldl max 0 keys
    in
    List.range 0 maxKey
        |> List.map
            (\i ->
                case Dict.get i cars of
                    Nothing ->
                        " "

                    Just _ ->
                        "X"
            )
        |> String.join ""


type alias Model =
    Dict.Dict Int Car


main =
    let
        cars =
            Dict.fromList
                [ ( 1
                  , { speed = 3
                    , pos = 1
                    }
                  )
                , ( 3
                  , { speed = 3
                    , pos = 3
                    }
                  )
                , ( 4
                  , { speed = 3
                    , pos = 4
                    }
                  )
                ]
    in
    Html.ul []
        [ Html.li []
            [ Html.text <|
                showCars cars
            ]
        , Html.li []
            [ Html.text <|
                showCars
                    (moveCars cars)
            ]
        , Html.li []
            [ Html.text <|
                showCars
                    (moveCars (moveCars cars))
            ]
        , Html.li []
            [ Html.text <|
                showCars
                    (moveCars (moveCars (moveCars cars)))
            ]
        ]
