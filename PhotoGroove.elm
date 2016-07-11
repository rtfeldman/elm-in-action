module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src ("http://elm-in-action.com/large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail selectedUrl thumbnail =
    img
        [ src ("http://elm-in-action.com/" ++ thumbnail.url)
        , classList [ ( "selected", selectedUrl == thumbnail.url ) ]
        ]
        []


model =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


main =
    view model
