module Photo exposing (Photo, PhotoUrl, url)


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
