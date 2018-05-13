module PhotoFolders exposing (main)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Photo exposing (Photo, PhotoUrl)


type Folder
    = Folder { name : String, photoUrls : List PhotoUrl, subfolders : List Folder, expanded : Bool }


type FolderPath
    = End
    | Descend Int FolderPath


type alias Model =
    { photos : Dict PhotoUrl Photo
    , selectedPhotoUrl : Maybe PhotoUrl
    , root : Folder
    }


type Msg
    = SelectPhoto PhotoUrl
    | LoadPage (Result Http.Error Model)
    | ToggleExpanded FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleExpanded path ->
            ( { model | root = toggleExpanded path model.root }, Cmd.none )

        SelectPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        LoadPage (Ok newModel) ->
            ( newModel, Cmd.none )

        LoadPage (Err _) ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , modelDecoder
        |> Http.get "http://elm-in-action.com/folders/list"
        |> Http.send LoadPage
    )


initialModel : Model
initialModel =
    { photos = Dict.empty
    , selectedPhotoUrl = Nothing
    , root = Folder { name = "Loading", expanded = True, photoUrls = [], subfolders = [] }
    }


modelDecoder : Decoder Model
modelDecoder =
    Json.Decode.succeed
        { selectedPhotoUrl = Just "coli"
        , photos =
            Dict.fromList
                [ ( "trevi", { title = "Trevi", related = [ "coli", "fresco" ], size = 34, url = "trevi" } )
                , ( "fresco", { title = "Fresco", related = [ "trevi" ], size = 46, url = "fresco" } )
                , ( "coli", { title = "Coliseum", related = [ "trevi", "fresco" ], size = 36, url = "coli" } )
                ]
        , root = initialRoot
        }


view : Model -> Html Msg
view model =
    let
        photoByUrl : PhotoUrl -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo
                        (List.filterMap photoByUrl photo.related)

                Nothing ->
                    text ""
    in
    div
        [ class "content" ]
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder model.root
            ]
        , div [ class "selected-photo" ] [ selectedPhoto ]
        ]


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


viewSelectedPhoto : Photo -> List Photo -> Html Msg
viewSelectedPhoto photo related =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ src (Photo.url ("photos/" ++ photo.url ++ "/full")) ] []
        , span [] [ text (toString photo.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , ul [ class "related-photos" ] (List.map viewRelatedPhoto related)
        ]


viewRelatedPhoto : Photo -> Html Msg
viewRelatedPhoto photo =
    li [ class "related-photo", onClick (SelectPhoto photo.url) ]
        [ div [] [ text photo.title ]
        , img
            [ class "related-photo"
            , src (Photo.url ("photos/" ++ photo.url ++ "/thumb"))
            ]
            []
        ]


viewFolder : Folder -> Html Msg
viewFolder (Folder folder) =
    let
        subfolders =
            List.map viewFolder folder.subfolders
    in
    div [ class "folder" ]
        (label [] [ text folder.name ] :: subfolders)


initialRoot : Folder
initialRoot =
    Folder
        { name = "Photos"
        , expanded = True
        , photoUrls = []
        , subfolders =
            [ Folder
                { name = "2016"
                , expanded = True
                , photoUrls = [ "trevi", "coli" ]
                , subfolders =
                    [ Folder { name = "outdoors", expanded = True, photoUrls = [], subfolders = [] }
                    , Folder { name = "indoors", expanded = True, photoUrls = [ "fresco" ], subfolders = [] }
                    ]
                }
            , Folder
                { name = "2017"
                , expanded = True
                , photoUrls = []
                , subfolders =
                    [ Folder { name = "outdoors", expanded = True, photoUrls = [], subfolders = [] }
                    , Folder { name = "indoors", expanded = True, photoUrls = [], subfolders = [] }
                    ]
                }
            ]
        }


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Descend targetIndex subPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentFolder =
                    if currentIndex == targetIndex then
                        toggleExpanded subPath currentFolder

                    else
                        currentFolder
            in
            Folder { folder | subfolders = subfolders }
