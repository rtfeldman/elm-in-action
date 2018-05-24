module PhotoFolders exposing (main)                                  

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)                      


type alias Model =
    { selectedPhotoUrl : Maybe String                                
    }

initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing }

init : ( Model, Cmd Msg )
init =
    ( initialModel                                                   
    , modelDecoder                                                   
        |> Http.get "http://elm-in-action.com/folders/list"          
        |> Http.send LoadPage                                        
    )

modelDecoder : Decoder Model
modelDecoder =
     Decode.succeed initialModel                                     

type Msg
    = SelectPhotoUrl String                                          
    | LoadPage (Result Http.Error Model)                             

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPhotoUrl url ->                                        
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )    

        LoadPage (Ok newModel) ->                                    
            ( newModel, Cmd.none )                                   

        LoadPage (Err _) ->                                          
            ( model, Cmd.none )                                      

view : Model -> Html Msg
view model =
    h1 [] [ text "The Grooviest Folders the world has ever seen" ]

main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }

