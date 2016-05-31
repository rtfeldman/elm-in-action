module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App
import String
import Task exposing (Task)
import Http


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


type alias Model =
    { photos : List Photo
    , selectedId : PhotoId
    , status : String
    }


model : Model
model =
    { photos = []
    , selectedId = -1
    , status = ""
    }


selectFirst : List Photo -> PhotoId
selectFirst photos =
    case List.head photos of
        Nothing ->
            -1

        Just photo ->
            photo.id


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

                selectedId =
                    selectFirst photos
            in
                ( { model | photos = photos, selectedId = selectedId }
                , Cmd.none
                )

        ReportError err ->
            ( { model | status = err }, Cmd.none )


handleLoadFailure : Http.Error -> Msg
handleLoadFailure _ =
    ReportError "HTTP error! (Have you tried turning it off and on again?)"


initialTask : Task Http.Error String
initialTask =
    Http.getString "http://elm-in-action.com/list-photos"


initialCmd : Cmd Msg
initialCmd =
    Task.perform handleLoadFailure LoadPhotos initialTask


main : Program Never
main =
    Html.App.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, initialCmd )
        }
