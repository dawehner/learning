module Main exposing (main)

import Browser
import Color
import Dict
import Element as E
import Element.Input as EI
import Html
import Html.Events
import Http
import Json.Decode as JD
import Json.Encode as JE
import Mindmap
import RemoteData as RD
import TypedSvg exposing (circle, svg)
import TypedSvg.Attributes exposing (cx, cy, fill, r, stroke, strokeWidth, viewBox)
import TypedSvg.Core
import TypedSvg.Events
import TypedSvg.Types exposing (Fill(..), px)


type alias Document =
    { id : Int
    , title : String
    }


type alias Model =
    { documents : RD.WebData (List Document)
    , newDocument : Maybe String
    , page : Page
    , mindmap : Maybe Mindmap.Nodes
    }


type Page
    = ListDocumentsPage
    | EditMindmapPage Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { documents = RD.NotAsked
      , newDocument = Nothing
      , page = EditMindmapPage 0
      , mindmap = Just Mindmap.exampleNodes
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
    | SwitchPage Page
    | AddChild Mindmap.Node


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
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/api/document/" ++ String.fromInt id
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
                    RD.map2 (::) document model.documents
              }
            , Cmd.none
            )

        DeleteDocument id ->
            ( model, deleteDocument id )

        DeleteDocumentResponse id ->
            ( { model | documents = RD.map (List.filter (.id >> (/=) id)) model.documents }, Cmd.none )

        SwitchPage page ->
            case page of
                ListDocumentsPage ->
                    ( model, fetchDocuments )

                EditMindmapPage id ->
                    ( Debug.todo "implement fetchFullDocument", Cmd.none )

        AddChild parent ->
            Maybe.map
                (\nodes ->
                    Mindmap.addNodeWithId
                        (Mindmap.emptyNode
                            |> Mindmap.withParent parent.id
                            |> Mindmap.withId (parent.id ++ "_" ++ String.fromInt (Mindmap.nodesToList nodes |> List.length))
                        )
                        nodes
                )
                model.mindmap
                |> (\nodes -> ( { model | mindmap = nodes }, Cmd.none ))


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \_ -> Sub.none
        }


keyCode : JD.Decoder Int
keyCode =
    JD.field "keyCode" JD.int


onKeyDown : (Int -> msg) -> E.Attribute msg
onKeyDown tagger =
    E.htmlAttribute (Html.Events.on "keydown" (JD.map tagger keyCode))


enterKey =
    13


viewDocuments : Maybe String -> RD.WebData (List Document) -> E.Element Msg
viewDocuments newDocument documents =
    RD.map
        (\ds ->
            List.map
                (\document ->
                    E.row [ E.spacing 10, E.width E.fill ]
                        [ E.text "title:"
                        , E.text document.title
                        , EI.button [ E.alignRight ]
                            { label = E.text "Delete"
                            , onPress = Just (DeleteDocument document.id)
                            }
                        ]
                )
                ds
                |> (::) (newDocumentBox newDocument)
                |> E.column
                    [ E.centerX
                    , E.spacing 20
                    , E.padding 40
                    , E.width (E.px 500)
                    ]
        )
        documents
        |> RD.withDefault (E.text "Loading ...")


newDocumentBox : Maybe String -> E.Element Msg
newDocumentBox maybeTitle =
    let
        title =
            Maybe.withDefault "" maybeTitle
    in
    E.row [ E.spacing 10, E.width E.fill ]
        [ EI.text
            [ onKeyDown
                (\i ->
                    if i == enterKey then
                        AddNewDocument

                    else
                        Noop
                )
            ]
            { label = EI.labelHidden "New document title"
            , onChange = UpdateNewDocumentTitle
            , placeholder = Nothing
            , text = title
            }
        , EI.button []
            { label = E.text "Add"
            , onPress = Just AddNewDocument
            }
        ]


view : Model -> Html.Html Msg
view model =
    E.layout [ E.width E.fill ]
        (case model.page of
            ListDocumentsPage ->
                viewDocuments model.newDocument model.documents

            EditMindmapPage _ ->
                case model.mindmap of
                    Just nodes ->
                        viewMindMap nodes

                    Nothing ->
                        E.none
        )


viewMindMap : Mindmap.Nodes -> E.Element Msg
viewMindMap nodes =
    let
        coords = Mindmap.maxNodes nodes
    in
    viewNodes nodes
        |> svg
            [ TypedSvg.Attributes.viewBox 0 0 (coords.x * 2) (coords.y * 2)
            ]
        |> E.html


viewNodes : Mindmap.Nodes -> List (TypedSvg.Core.Svg Msg)
viewNodes (Mindmap.Nodes nodes) =
    Dict.map (always viewNode) nodes
        |> Dict.values
        |> List.concat


viewNode : Mindmap.Node -> List (TypedSvg.Core.Svg Msg)
viewNode node =
    [ TypedSvg.ellipse
        [ TypedSvg.Attributes.cx (TypedSvg.Types.px node.x)
        , TypedSvg.Attributes.cy (TypedSvg.Types.px node.y)
        , TypedSvg.Attributes.ry (TypedSvg.Types.px 15)
        , TypedSvg.Attributes.rx (TypedSvg.Types.px node.width)
        ]
        []
    , TypedSvg.circle
        [ TypedSvg.Attributes.cx (TypedSvg.Types.px (node.x + 80))
        , TypedSvg.Attributes.cy (TypedSvg.Types.px node.y)
        , TypedSvg.Attributes.r (TypedSvg.Types.px 10)
        , TypedSvg.Events.onClick (AddChild node)
        ]
        []
    , TypedSvg.text_
        [ TypedSvg.Attributes.x (TypedSvg.Types.px (node.x - (node.width / 2)))
        , TypedSvg.Attributes.y (TypedSvg.Types.px node.y)
        , TypedSvg.Attributes.fill (TypedSvg.Types.Fill Color.white)
        ]
        [ TypedSvg.Core.text node.text ]
    ]
