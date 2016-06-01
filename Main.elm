module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App
import String
import Task
import Http


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        ]


viewThumbnail selectedUrl thumbnail =
    img
        [ src thumbnail.url
        , classList [ ( "selected", selectedUrl == thumbnail.url ) ]
        , onClick { operation = "SELECT_PHOTO", data = thumbnail.url }
        ]
        []


model : { photos : List { url : String }, selectedUrl : String }
model =
    { photos = [], selectedUrl = "" }


update msg model =
    if msg.operation == "SELECT_PHOTO" then
        ( { model | selectedUrl = msg.data }, Cmd.none )
    else if msg.operation == "LOAD_PHOTOS" then
        let
            urls =
                String.split "\n" msg.data

            photos =
                List.map (\url -> { url = url }) urls
        in
            ( { model | photos = photos }, Cmd.none )
    else
        ( model, Cmd.none )


handleLoadSuccess data =
    { operation = "LOAD_PHOTOS", data = data }


handleLoadFailure _ =
    { operation = "REPORT_ERROR"
    , data = "HTTP error! (Have you tried turning it off and on again?)"
    }


photoListUrl : String
photoListUrl =
    "http://elm-in-action.com/list-photos"


initialTask =
    Http.getString photoListUrl


initialCmd =
    Task.perform handleLoadFailure handleLoadSuccess initialTask


main =
    Html.App.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, initialCmd )
        }
