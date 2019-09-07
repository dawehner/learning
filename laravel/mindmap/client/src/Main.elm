module Main exposing (main)

import Html
import Browser
import Element as E
import Element.Input as EI
import Http
import Json.Decode as JD
import Json.Encode as JE
import RemoteData as RD


type alias Document =
    { id : Int
    , title : String
    }


type alias Model =
    { documents : RD.WebData (List Document)
    , newDocument : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { documents = RD.NotAsked
      , newDocument = Nothing
      }
    , fetchDocuments
    )


type Msg
    = Noop
    | DocumentListResponse (RD.WebData (List Document))
    | UpdateNewDocumentTitle String
    | AddNewDocument
    | AddNewDocumentResponse (RD.WebData Document)
    | DeleteDocument Int
    | DeleteDocumentResponse Int


fetchDocuments : Cmd Msg
fetchDocuments =
    Http.get
        { expect = Http.expectJson (RD.fromResult >> DocumentListResponse) decodeDocuments
        , url = "/api/document"
        }


addNewDocument : String -> Cmd Msg
addNewDocument title =
    Http.post
        { expect = Http.expectJson (RD.fromResult >> AddNewDocumentResponse) decodeDocument
        , url = "/api/document"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "title", JE.string title )
                    ]
                )
        }

deleteDocument : Int -> Cmd Msg
deleteDocument id =
  Http.request {
    method = "DELETE"
    , headers = []
    , url = "/api/document/" ++ (String.fromInt id)
    , expect = Http.expectWhatever (always (DeleteDocumentResponse id))
    , body = Http.emptyBody
    , timeout = Nothing
    , tracker = Nothing
  }

decodeDocuments : JD.Decoder (List Document)
decodeDocuments =
    JD.list decodeDocument


decodeDocument : JD.Decoder Document
decodeDocument =
    JD.map2 Document
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        DocumentListResponse documents ->
            ( { model | documents = Debug.log "documents" documents }, Cmd.none )

        UpdateNewDocumentTitle string ->
            ( { model | newDocument = Just string }, Cmd.none )

        AddNewDocument ->
            case model.newDocument of
                Nothing ->
                    ( model, Cmd.none )

                Just title ->
                    ( { model | newDocument = Nothing }, addNewDocument title )

        AddNewDocumentResponse document ->
            ( { model
                | documents =
                    RD.map2 ((::)) document model.documents
              }
            , Cmd.none
            )

        DeleteDocument id ->
            (model, deleteDocument id)

        DeleteDocumentResponse id ->
            ({ model | documents = RD.map (List.filter (.id >> (/=) id)) model.documents}, Cmd.none)




main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \_ -> Sub.none
        }


viewDocuments : Maybe String -> RD.WebData (List Document) -> E.Element Msg
viewDocuments newDocument documents =
    RD.map
        (\ds ->
            List.map
                (\document ->
                    E.row [ E.spacing 10 ]
                        [ E.text "title:"
                        , E.text document.title
                        , EI.button [E.alignRight] {
                        label = E.text "Delete"
                          , onPress= Just (DeleteDocument document.id)
                        }
                        ]
                )
                ds
                |> (::) (newDocumentBox newDocument)
                |> E.column [ E.centerX, E.spacing 20, E.padding 40 ]
        )
        documents
        |> RD.withDefault (E.text "Loading ...")

newDocumentBox : Maybe String -> E.Element Msg
newDocumentBox maybeTitle =
    let
        title = Maybe.withDefault "" maybeTitle
    in
      E.row [E.spacing 10] [
      EI.text [] {
        label = EI.labelHidden "New document title"
        , onChange = UpdateNewDocumentTitle
        , placeholder = Nothing
        , text = title
      },
      EI.button [] {
        label = E.text "Add"
        , onPress = Just AddNewDocument
      }
      ]


view : Model -> Html.Html Msg
view model =
    E.layout [ E.width E.fill ]
        (viewDocuments model.newDocument model.documents )
