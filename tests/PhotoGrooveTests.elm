module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import PhotoGroove exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")


stateTransitions : Test
stateTransitions =
    describe "state transitions"
        [ fuzz string "SelectByUrl selects the given photo by URL" <|
            \url ->
                PhotoGroove.initialModel
                    |> PhotoGroove.update (SelectByUrl url)
                    |> Tuple.first
                    |> .selectedUrl
                    |> Expect.equal (Just url)
        , fuzz (list string) "LoadPhotos selects the first photo" <|
            \urls ->
                let
                    photos =
                        List.map photoFromUrl urls
                in
                    PhotoGroove.initialModel
                        |> PhotoGroove.update (LoadPhotos (Ok photos))
                        |> Tuple.first
                        |> .selectedUrl
                        |> Expect.equal (List.head urls)
        ]


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }
