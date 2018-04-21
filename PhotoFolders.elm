module PhotoFolders exposing (main)

import Dict exposing (Dict)
import Folder exposing (Folder(..), FolderPath(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Photo exposing (Photo, PhotoUrl)


type alias Model =
    { photos : Dict PhotoUrl Photo
    , selectedPhotoUrl : Maybe PhotoUrl
    , root : Folder
    }


type Msg
    = SelectPhoto PhotoUrl
    | ToggleExpanded FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        ToggleExpanded path ->
            ( { model | root = Folder.toggleExpanded path model.root }, Cmd.none )


photos : Dict PhotoUrl Photo
photos =
    Dict.fromList
        [ ( "trevi.png", { title = "Trevi", related = [ "coli.png", "fresco.png" ], size = 174, url = "trevi.png" } )
        , ( "fresco.png", { title = "Fresco", related = [ "trevi.png" ], size = 231, url = "fresco.png" } )
        , ( "coli.png", { title = "Coliseum", related = [ "trevi.png" ], size = 149, url = "coli.png" } )
        ]


init : ( Model, Cmd Msg )
init =
    ( { photos = photos, selectedPhotoUrl = Just "trevi.png", root = Folder.initialRoot }
    , Cmd.none
    )


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
    div [ class "content" ]
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder End model.root
            ]
        , div [ class "selected-photo" ] [ selectedPhoto ]
        ]


viewPhoto : PhotoUrl -> Html Msg
viewPhoto url =
    div [ class "photo", onClick (SelectPhoto url) ]
        [ text url ]


viewSelectedPhoto : Photo -> List Photo -> Html Msg
viewSelectedPhoto photo related =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ title photo.title, src photo.url ] []
        , h3 [] [ text "Related" ]
        , ul [ class "related-photoUrls" ] (List.map viewRelatedPhoto related)
        ]


viewRelatedPhoto : Photo -> Html Msg
viewRelatedPhoto photo =
    li [ class "related-photo", onClick (SelectPhoto photo.url) ]
        [ div [] [ text photo.title ]
        , img [ class "related-photo", title photo.title, src photo.url ] []
        ]


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubfolder index subfolder =
            viewFolder (Subfolder index path) subfolder

        folderLabel =
            label [ onClick (ToggleExpanded path) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            contents =
                List.append
                    (List.indexedMap viewSubfolder folder.subfolders)
                    (List.map viewPhoto folder.photoUrls)
        in
        div [ class "folder expanded" ] (folderLabel :: contents)

    else
        div [ class "folder collapsed" ] [ folderLabel ]


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


modelDecoder : Decoder Model
modelDecoder =
    Folder.folderAndPhotos
        |> Decode.map toModel


toModel : ( Folder, Dict PhotoUrl Photo ) -> Model
toModel ( root, photos ) =
    { photos = photos
    , root = root
    , selectedPhotoUrl = Nothing
    }
