module PhotoFolders exposing (main)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Photo exposing (Photo, PhotoUrl)


type alias Model =
    { photos : Dict PhotoUrl Photo
    , selectedPhotoUrl : Maybe PhotoUrl
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
    ( { photos = Dict.empty, selectedPhotoUrl = Nothing }
    , modelDecoder
        |> Http.get "http://elm-in-action.com/folders/list"
        |> Http.send LoadPage
    )


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
        }


view : Model -> Html Msg
view model =
    h1 [] [ text "TODO: Implement this page!" ]


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }
