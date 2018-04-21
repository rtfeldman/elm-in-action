module Photo exposing (Photo, PhotoUrl, decoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias PhotoUrl =
    String


type alias Photo =
    { title : String
    , size : Int
    , related : List PhotoUrl
    , url : PhotoUrl
    }


decoder : Decoder (Dict PhotoUrl Photo)
decoder =
    Decode.keyValuePairs partialDecoder
        |> Decode.map fromPairs


fromPairs : List ( PhotoUrl, PhotoUrl -> Photo ) -> Dict PhotoUrl Photo
fromPairs pairs =
    List.map (\( url, photoFromUrl ) -> ( url, photoFromUrl url )) pairs
        |> Dict.fromList


partialDecoder : Decoder (PhotoUrl -> Photo)
partialDecoder =
    Decode.succeed Photo
        |> required "title" string
        |> required "size" int
        |> required "related_photos" (list string)
