module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element as E
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Matrix
import Maybe.Extra
import Random
import Task
import Time



---- MODEL ----


type alias Model =
    { grid : Matrix.Matrix
    , seed : Random.Seed
    }


init : ( Model, Cmd Msg )
init =
    let
        rows =
            100

        columns =
            100
    in
    ( { grid =
            Matrix.fromList
                ( rows
                , columns
                )
                (List.range 1 (rows * columns) |> List.map (\_ -> 0))
      , seed = Random.initialSeed 0
      }
    , Random.generate GeneratedMatrix (generateMatrix rows columns)
    )


generateMatrix : Int -> Int -> Random.Generator Matrix.Matrix
generateMatrix rows columns =
    Random.map (Matrix.fromList ( rows, columns ))
        (Random.list (rows * columns) (Random.uniform 1.0 [ -1.0 ]))



---- UPDATE ----


type Msg
    = NoOp
    | GeneratedMatrix Matrix.Matrix
    | ChangedTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GeneratedMatrix matrix ->
            ( { model | grid = matrix }, Cmd.none )

        ChangedTime _ ->
            let
                ( matrix_, seed_ ) =
                    easingStep model.seed model.grid
            in
            ( { model
                | grid = matrix_
                , seed = seed_
              }
            , Cmd.none
            )


expE =
    2.718


easingStep : Random.Seed -> Matrix.Matrix -> ( Matrix.Matrix, Random.Seed )
easingStep seed matrix =
    let
        ( rows, columns ) =
            Matrix.size matrix

        tempT =
            1.0
    in
    List.range 1 rows
        |> List.foldl
            (\i ( matrix_, seed_ ) ->
                List.range 1 columns
                    |> List.foldl
                        (\j ( matrix__, seed__ ) ->
                            -- Get all available neighbords
                            let
                                s0 =
                                    Matrix.get ( i, j ) matrix
                                        |> Maybe.withDefault 0.0

                                -- |> Debug.log "s0"
                                sn =
                                    [ Matrix.get ( i + 1, j ) matrix
                                    , Matrix.get ( i - 1, j ) matrix
                                    , Matrix.get ( i, j + 1 ) matrix
                                    , Matrix.get ( i, j - 1 ) matrix
                                    ]
                                        |> Maybe.Extra.combine
                                        |> Maybe.withDefault []
                                        |> List.sum

                                beta =
                                    2.0

                                deltaE =
                                    -beta * sn * s0

                                z =
                                    expE ^ (-deltaE / tempT)

                                ( rand, seed___ ) =
                                    Random.step (Random.float 0.0 1.0) seed__
                            in
                            if rand < z then
                                ( Matrix.set ( i, j ) -s0 matrix__, seed___ )

                            else
                                ( matrix__, seed___ )
                        )
                        ( matrix_, seed_ )
            )
            ( matrix, seed )



---- VIEW ----


black =
    E.rgb 0 0 0


white =
    E.rgb 1 1 1


grid : Matrix.Matrix -> E.Element msg
grid matrix =
    let
        ( n, m ) =
            Matrix.size matrix

        rows =
            Matrix.to2DList matrix
    in
    rows
        |> List.map
            (\row ->
                row
                    |> List.map
                        (\item ->
                            E.el
                                [ E.width (E.px 8)
                                , E.height (E.px 8)
                                , E.centerX
                                , E.centerY
                                , Border.solid
                                , Border.width 1
                                , (if item > 0.5 then
                                    white

                                   else
                                    black
                                  )
                                    |> Background.color
                                ]
                                (E.text "")
                        )
                    |> E.row
                        []
            )
        |> E.column
            []


view : Model -> Html Msg
view model =
    E.layout [ E.width E.fill, E.height E.fill ]
        (grid model.grid)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always (Time.every 50 ChangedTime)
        }
