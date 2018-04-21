module Folder exposing (Folder(..), FolderPath(..), folderAndPhotos, initialRoot, toggleExpanded)

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


folderAndPhotos : Decoder ( Folder, Photos )
folderAndPhotos =
    Decode.succeed assemble
        |> required "name" Decode.string
        |> required "photos" Photo.decoder
        |> required "subfolders" (Decode.list (Decode.lazy (\() -> folderAndPhotos)))


assemble :
    String
    -> Photos
    -> List ( Folder, Photos )
    -> ( Folder, Photos )
assemble folderName folderPhotos subfolderPairs =
    let
        ( subfolders, subfolderPhotos ) =
            List.unzip subfolderPairs

        folder : Folder
        folder =
            Folder
                { expanded = True
                , photoUrls = Dict.keys folderPhotos
                , subfolders = subfolders
                , name = folderName
                }

        photos : Photos
        photos =
            List.foldr Dict.union folderPhotos subfolderPhotos
    in
    ( folder, photos )


initialRoot : Folder
initialRoot =
    Folder
        { name = "Photos"
        , expanded = True
        , photoUrls = []
        , subfolders =
            [ Folder
                { name = "2016"
                , photoUrls = []
                , expanded = True
                , subfolders =
                    [ Folder { name = "outdoors", expanded = True, photoUrls = [], subfolders = [] }
                    , Folder { name = "indoors", expanded = True, photoUrls = [], subfolders = [] }
                    ]
                }
            , Folder
                { name = "2017"
                , photoUrls = []
                , expanded = True
                , subfolders =
                    [ Folder { name = "outdoors", expanded = True, photoUrls = [], subfolders = [] }
                    , Folder { name = "indoors", expanded = True, photoUrls = [], subfolders = [] }
                    ]
                }
            ]
        }


json : String
json =
    """
      {
        "name": "Photos"
        "photos": {},
        "subfolders": [
            {
                "name": "2016",
                "photos": {},
                "subfolders": [
                    {
                        "name": "outdoors",
                        "photos": {},
                        "subfolders": {}
                    },
                    {
                        "name": "indoors",
                        "photos": {},
                        "subfolders": {}
                    }
                }
            },
            {
                "name": "2017",
                "photos": {},
                "subfolders": [
                    {
                        "name": "outdoors",
                        "photos": {},
                        "subfolders": {}
                    },
                    {
                        "name": "indoors",
                        "photos": {},
                        "subfolders": {}
                    }
                ]
            }
        ]
    }
    """
