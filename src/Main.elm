module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)


type alias Model =
    {}


view : Model -> Document Msg
view model =
    { title = "Photo Groove, SPA Style"
    , body = [ text "This isn't even my final form!" ]
    }


type Msg
    = NothingYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( {}, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
