port module Main exposing (..)

import Char exposing (isDigit)
import Html
import Parser exposing (..)


type alias Model =
    {}


type alias Hostfile =
    List HostEntry


type HostEntry
    = Comment String
    | Entry Ip



-- (List HostAlias)


type alias Ipv4 =
    { k1 : Int
    , k2 : Int
    , k3 : Int
    , k4 : Int
    }


type alias Ipv6 =
    List Int


type Ip
    = Ip4 Ipv4
    | Ip6 Ipv6


type HostAlias
    = HostAlias String


type Msg
    = Noop
    | ParseHostFile String


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


port parseHostFile : (String -> msg) -> Sub msg


ip4Parser : Parser Ipv4
ip4Parser =
    succeed Ipv4
        |= intRange 0 255
        |. symbol "."
        |= intRange 0 255
        |. symbol "."
        |= intRange 0 255
        |. symbol "."
        |= intRange 0 255


hexBlock : Parser (List Int)
hexBlock =
    (getChompedString <|
        succeed ()
            |. chompWhile
                (\c ->
                    Char.isDigit c
                        || (c
                                == 'A'
                           )
                        || (c
                                == 'B'
                           )
                        || (c
                                == 'C'
                           )
                        || (c
                                == 'D'
                           )
                        || (c
                                == 'E'
                           )
                        || (c
                                == 'F'
                           )
                )
    )
        |> andThen
            (xs ->
            List.map 
            )
            (\x ->
                case x of
                    "0" ->
                        succeed 0

                    "1" ->
                        succeed 1

                    "2" ->
                        succeed 2

                    "3" ->
                        succeed 3

                    "4" ->
                        succeed 4

                    "5" ->
                        succeed 5

                    "6" ->
                        succeed 6

                    "7" ->
                        succeed 7

                    "8" ->
                        succeed 8

                    "9" ->
                        succeed 9

                    "A" ->
                        succeed 10

                    "B" ->
                        succeed 11

                    "C" ->
                        succeed 12

                    "D" ->
                        succeed 13

                    "E" ->
                        succeed 14

                    "F" ->
                        succeed 15

                    y ->
                        let
                            a =
                                Debug.log "y" y
                        in
                        problem "invalid char"
            )


ip6Parser : Parser Ipv6
ip6Parser =
    succeed (\a b c d e f g h -> [ a, b, c, d, e, f, g, h ])
        |= hex
        |= hex
        |= hex
        |= hex
        |. symbol ":"
        |= hex
        |= hex
        |= hex
        |= hex



-- |. symbol ":"
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255
-- |. symbol ":"
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255
-- |. symbol ":"
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255
-- |. symbol ":"
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255
-- |. symbol ":"
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255
-- |. symbol ":"
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255
-- |= intRange 0 255


intRange : Int -> Int -> Parser Int
intRange from to =
    getChompedString (chompWhile Char.isDigit)
        |> andThen (checkRange from to)


checkRange : Int -> Int -> String -> Parser Int
checkRange from to string =
    case String.toInt string of
        Nothing ->
            problem "String is not an integer"

        Just x ->
            if x > to then
                problem "Number too big"

            else if x < from then
                problem "Number too small"

            else
                succeed x



-- |. end
--
-- |= int
-- |. symbol "."
-- |= int
-- |. symbol "."
-- |= int


keepLineComment : String -> Parser HostEntry
keepLineComment str =
    succeed Comment
        |. symbol str
        |. spaces
        |= (getChompedString <| chompWhile (\c -> c /= '\n'))
        |. oneOf [ symbol "\n", end ]


hostFileParser : Parser Hostfile
hostFileParser =
    loop [] hostFileParserHelper
        |. end


hostFileParserHelper : Hostfile -> Parser (Step Hostfile Hostfile)
hostFileParserHelper hs =
    oneOf
        [ succeed (\entry -> Loop (entry :: hs))
            |= oneOf [ ip4Parser |> map Ip4 |> map Entry, keepLineComment "#" ]
        , succeed () |> map (\_ -> Done hs)
        ]


executeParseHostfile : String -> Result (List Parser.DeadEnd) Hostfile
executeParseHostfile string =
    Parser.run
        hostFileParser
        string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ParseHostFile string ->
            let
                a_ =
                    Debug.log "meh" (executeParseHostfile string)
            in
            ( model, Cmd.none )


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = \_ -> parseHostFile ParseHostFile
        }
