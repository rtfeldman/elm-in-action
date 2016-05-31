module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App
import String


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


main =
    Html.App.program
        { view = view
        , update = update
        , init = ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
