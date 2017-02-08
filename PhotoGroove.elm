module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Array exposing (Array)
import Random
import Http
import Html.Attributes as Attr exposing (id, class, classList, src, name, type_, title)
import Json.Decode exposing (string, int, list, Decoder, at)
import Json.Decode.Pipeline exposing (decode, required, optional)


photoDecoder : Decoder Photo
photoDecoder =
    decode buildPhoto
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


buildPhoto : String -> Int -> String -> Photo
buildPhoto url size title =
    { url = url, size = size, title = title }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type ThumbnailSize
    = Small
    | Medium
    | Large


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick SurpriseMe ]
            [ text "Surprise Me!" ]
        , div [ class "filters" ]
            [ viewFilter "Hue" SetHue model.hue
            , viewFilter "Ripple" SetRipple model.ripple
            , viewFilter "Noise" SetNoise model.noise
            ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , viewLarge model.selectedUrl
        ]


viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name toMsg magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , paperSlider [ Attr.max "11", onImmediateValueChange toMsg ] []
        , label [] [ text (toString magnitude) ]
        ]


viewLarge : Maybe String -> Html Msg
viewLarge maybeUrl =
    case maybeUrl of
        Nothing ->
            text ""

        Just url ->
            img [ class "large", src (urlPrefix ++ "large/" ++ url) ] []


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , title (thumbnail.title ++ " [" ++ toString thumbnail.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (SetSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


initialModel : Model
initialModel =
    { photos = []
    , selectedUrl = Nothing
    , loadingError = Nothing
    , chosenSize = Medium
    , hue = 0
    , ripple = 0
    , noise = 0
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


getPhotoUrl : Int -> Maybe String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            Just photo.url

        Nothing ->
            Nothing


type Msg
    = SelectByUrl String
    | SelectByIndex Int
    | SurpriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error (List Photo))
    | SetHue Int
    | SetRipple Int
    | SetNoise Int


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetHue hue ->
            ( { model | hue = hue }
            , Cmd.none
            )

        SetRipple ripple ->
            ( { model | ripple = ripple }
            , Cmd.none
            )

        SetNoise noise ->
            ( { model | noise = noise }
            , Cmd.none
            )

        SelectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
                ( { model | selectedUrl = newSelectedUrl }, Cmd.none )

        SelectByUrl url ->
            ( { model | selectedUrl = Just url }, Cmd.none )

        SurpriseMe ->
            let
                randomPhotoPicker =
                    Random.int 0 (List.length model.photos - 1)
            in
                ( model, Random.generate SelectByIndex randomPhotoPicker )

        SetSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        LoadPhotos (Ok photos) ->
            ( { model
                | photos = photos
                , selectedUrl = Maybe.map .url (List.head photos)
              }
            , Cmd.none
            )

        LoadPhotos (Err _) ->
            ( { model
                | loadingError = Just "Error! (Try turning it off and on again?)"
              }
            , Cmd.none
            )


initialCmd : Cmd Msg
initialCmd =
    list photoDecoder
        |> Http.get "http://elm-in-action.com/photos/list.json"
        |> Http.send LoadPhotos


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just errorMessage ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Groove" ]
                , p [] [ text errorMessage ]
                ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider =
    node "paper-slider"


onImmediateValueChange : (Int -> msg) -> Attribute msg
onImmediateValueChange toMsg =
    at [ "target", "immediateValue" ] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed"
