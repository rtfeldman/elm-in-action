module PhotoFolders exposing (main)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Photo exposing (Photo, PhotoUrl)


type Folder
    = Folder { name : String, photoUrls : List PhotoUrl, subfolders : List Folder }


type alias Model =
    { photos : Dict PhotoUrl Photo
    , selectedPhotoUrl : Maybe PhotoUrl
    , root : Folder
    }


type Msg
    = SelectPhoto PhotoUrl
    | LoadPage (Result Http.Error Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
    , root = Folder { name = "Loading", photoUrls = [], subfolders = [] }
    }


modelDecoder : Decoder Model
modelDecoder =
    Json.Decode.succeed
        { selectedPhotoUrl = Just "trevi.jpg"
        , photos =
            Dict.fromList
                [ ( "trevi.jpg", { title = "Trevi", related = [ "coli.jpg", "fresco.jpg" ], size = 174, url = "trevi.jpg" } )
                , ( "fresco.jpg", { title = "Fresco", related = [ "trevi.jpg" ], size = 231, url = "fresco.jpg" } )
                , ( "coli.jpg", { title = "Coliseum", related = [ "trevi.jpg" ], size = 149, url = "coli.jpg" } )
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
    div [ class "content" ]
        [ div [ class "selected-photo" ] [ selectedPhoto ] ]


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


viewSelectedPhoto : Photo -> List Photo -> Html Msg
viewSelectedPhoto photo related =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ title photo.title, src photo.url ] []
        , h3 [] [ text "Related" ]
        , ul [ class "related-photos" ] (List.map viewRelatedPhoto related)
        ]


viewRelatedPhoto : Photo -> Html Msg
viewRelatedPhoto photo =
    li [ class "related-photo", onClick (SelectPhoto photo.url) ]
        [ div [] [ text photo.title ]
        , img [ class "related-photo", title photo.title, src photo.url ] []
        ]


initialRoot : Folder
initialRoot =
    Folder
        { name = "Photos"
        , photoUrls = []
        , subfolders =
            [ Folder
                { name = "2016"
                , photoUrls = []
                , subfolders =
                    [ Folder { name = "outdoors", photoUrls = [], subfolders = [] }
                    , Folder { name = "indoors", photoUrls = [], subfolders = [] }
                    ]
                }
            , Folder
                { name = "2017"
                , photoUrls = []
                , subfolders =
                    [ Folder { name = "outdoors", photoUrls = [], subfolders = [] }
                    , Folder { name = "indoors", photoUrls = [], subfolders = [] }
                    ]
                }
            ]
        }
