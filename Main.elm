module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App


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


model =
    { photos =
        [ { url = "http://elm-in-action.com/1.jpeg" }
        , { url = "http://elm-in-action.com/2.jpeg" }
        , { url = "http://elm-in-action.com/3.jpeg" }
        ]
    , selectedUrl = "http://elm-in-action.com/1.jpeg"
    }


update msg model =
    if msg.operation == "SELECT_PHOTO" then
        { model | selectedUrl = msg.data }
    else
        model


main =
    Html.App.beginnerProgram { model = model, view = view, update = update }
