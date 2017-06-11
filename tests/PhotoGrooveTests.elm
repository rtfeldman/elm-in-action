module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import PhotoGroove exposing (..)
import Json.Decode exposing (decodeString)


decoderTest : Test
decoderTest =
    test "title defaults to (untitled)" <|
        \_ ->
            """{"url": "foo.com", "size": 5}"""
                |> decodeString photoDecoder
                |> Expect.equal (Ok { url = "foo.com", size = 5, title = "(untitled)" })
