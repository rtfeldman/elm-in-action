module Folder exposing (Folder(..), FolderPath(..), folderDecoder, initialRoot, photosDecoder, toggleExpanded)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Photo exposing (Photo, PhotoUrl)


type Folder
    = Folder { expanded : Bool, photoUrls : List PhotoUrl, subfolders : List Folder, name : String }


type FolderPath
    = End
    | Subfolder Int FolderPath


type alias Photos =
    Dict PhotoUrl Photo


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex subPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentFolder =
                    if currentIndex == targetIndex then
                        toggleExpanded subPath currentFolder

                    else
                        currentFolder
            in
            Folder { folder | subfolders = subfolders }


folderDecoder : Decoder Folder
folderDecoder =
    Decode.succeed folderFromJson
        |> required "name" Decode.string
        |> required "photos" Photo.decoder
        |> required "subfolders" (Decode.list (Decode.lazy (\() -> folderDecoder)))


folderFromJson :
    String
    -> Dict PhotoUrl Photo
    -> List Folder
    -> Folder
folderFromJson name folderPhotos subfolders =
    Folder
        { name = name
        , expanded = True
        , photoUrls = Dict.keys folderPhotos
        , subfolders = subfolders
        }


photosDecoder : Decoder (Dict PhotoUrl Photo)
photosDecoder =
    Decode.succeed photosFromJson
        |> required "photos" Photo.decoder
        |> required "subfolders" (Decode.list (Decode.lazy (\() -> photosDecoder)))


photosFromJson : Dict PhotoUrl Photo -> List (Dict PhotoUrl Photo) -> Dict PhotoUrl Photo
photosFromJson photos subfolderPhotos =
    List.foldl Dict.union photos subfolderPhotos
