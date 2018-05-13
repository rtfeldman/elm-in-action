module Photo exposing (Photo, PhotoUrl)


type alias PhotoUrl =
    String


type alias Photo =
    { title : String
    , related : List PhotoUrl
    , size : Int
    , url : PhotoUrl
    }
