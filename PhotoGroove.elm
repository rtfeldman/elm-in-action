module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App
import Array exposing (Array)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type alias Msg =
    { operation : String, data : String }


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick { operation = "SURPRISE_ME", data = "" } ]
            [ text "Surprise Me!" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , classList [ ( "selected", selectedUrl == thumbnail.url ) ]
        , onClick { operation = "SELECT_PHOTO", data = thumbnail.url }
        ]
        []


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    }


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


update : Msg -> Model -> Model
update msg model =
    if msg.operation == "SELECT_PHOTO" then
        { model | selectedUrl = msg.data }
    else if msg.operation == "SURPRISE_ME" then
        { model | selectedUrl = "2.jpeg" }
    else
        model


main =
    Html.App.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
