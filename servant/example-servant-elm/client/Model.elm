module Model exposing (..)

import Dict exposing (Dict)
import Result
import Http


type alias Model =
    { items : Dict Int Item
    , addItemInput : String
    , error : Maybe String
    }


type alias Item =
    { id : Int
    , text : String
    }


type alias ItemId =
    Int


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String


type FromServer
    = Initial (List ItemId)
    | NewItem Item
    | Delete ItemId


type FromUi
    = AddItemInputChange String
    | AddItemButton
    | Done ItemId


convertResult : Result (Http.Error a) -> Result (Error a)
convertResult =
    Result.mapError (Error << toString)
