module Photo exposing (Photo, PhotoUrl, url)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type alias PhotoUrl =
    String


type alias Photo =
    { title : String
    , related : List PhotoUrl
    , size : Int
    , url : PhotoUrl
    }


url : String -> PhotoUrl
url suffix =
    "http://elm-in-action.com/" ++ suffix
