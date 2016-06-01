module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App
import String
import Task
import Http


type alias Model =
    { photos : List Photo, selectedId : Int, status : String }


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedId) model.photos)
        , div [] [ text model.status ]
        ]


viewThumbnail : PhotoId -> Photo -> Html Msg
viewThumbnail selectedId thumbnail =
    img
        [ src thumbnail.url
        , classList [ ( "selected", selectedId == thumbnail.id ) ]
        , onClick (SelectPhoto thumbnail.id)
        ]
        []


type alias PhotoId =
    Int


type alias Photo =
    { id : PhotoId
    , url : String
    }


model : Model
model =
    { photos = [], selectedId = -1, status = "" }


type Msg
    = LoadPhotos String
    | SelectPhoto PhotoId
    | ReportError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPhoto photoId ->
            ( { model | selectedId = photoId }, Cmd.none )

        LoadPhotos str ->
            let
                urls : List String
                urls =
                    String.split "\n" str

                photos : List Photo
                photos =
                    List.indexedMap (\id url -> { id = id, url = url }) urls

                selectedId : PhotoId
                selectedId =
                    selectFirstId photos
            in
                ( { model | photos = photos, selectedId = selectedId }
                , Cmd.none
                )

        ReportError err ->
            ( { model | status = err }, Cmd.none )


selectFirstId : List Photo -> PhotoId
selectFirstId photos =
    case List.head photos of
        Just photo ->
            photo.id

        Nothing ->
            -1


handleLoadFailure : Http.Error -> Msg
handleLoadFailure _ =
    ReportError "HTTP error! (Have you tried turning it off and on again?)"


photoListUrl : String
photoListUrl =
    "http://elm-in-action.com/list-photos"


initialTask =
    Http.getString photoListUrl


initialCmd =
    Task.perform handleLoadFailure LoadPhotos initialTask


main =
    Html.App.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, initialCmd )
        }
