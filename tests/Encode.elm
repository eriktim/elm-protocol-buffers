module Encode exposing (suite)

import Bytes.Encode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Hex
import ProtoBuf.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "Should correctly encode"
        [ Encode.message [ ( 2 ^ 29 - 1, Encode.int32 1 ) ]
            |> expectBytes "F8FFFFFF0F01"
            |> test "field numbers"
        , Encode.message [ ( 1, Encode.double 0.01 ) ]
            |> expectBytes "097B14AE47E17A843F"
            |> test "a double"
        , Encode.message [ ( 1, Encode.float 0.01 ) ]
            |> expectBytes "0D0AD7233C"
            |> test "a float"
        , Encode.message [ ( 1, Encode.int32 150 ) ]
            |> expectBytes "089601"
            |> test "an int32"
        , Encode.message [ ( 1, Encode.int32 -150 ) ]
            |> expectBytes "08EAFEFFFF0F"
            |> test "a negative int32"
        , Encode.message [ ( 1, Encode.uint32 (2 ^ 32 - 1) ) ]
            |> expectBytes "08FFFFFFFF0F"
            |> test "an uint32"
        , Encode.message [ ( 1, Encode.sint32 150 ) ]
            |> expectBytes "08AC02"
            |> test "a sint32"
        , Encode.message [ ( 1, Encode.sint32 -150 ) ]
            |> expectBytes "08AB02"
            |> test "a negative sint32"
        , Encode.message [ ( 1, Encode.fixed32 150 ) ]
            |> expectBytes "0D96000000"
            |> test "an fixed32"
        , Encode.message [ ( 1, Encode.sfixed32 (2 ^ 32 - 1) ) ]
            |> expectBytes "0DFFFFFFFF"
            |> test "a sfixed32"
        , Encode.message [ ( 1, Encode.sfixed32 -150 ) ]
            |> expectBytes "0D6AFFFFFF"
            |> test "a negative sfixed32"
        , Encode.message [ ( 2, Encode.string "testing" ) ]
            |> expectBytes "120774657374696E67"
            |> test "a string"
        , Encode.message [ ( 3, Encode.embedded Encode.message (Just [ ( 1, Encode.int32 150 ) ]) ) ]
            |> expectBytes "1A03089601"
            |> test "an embedded message"
        , Encode.message [ ( 3, Encode.embedded Encode.message Nothing ) ]
            |> expectBytes ""
            |> test "an empty embedded message"
        , Encode.message [ ( 4, Encode.repeated Encode.int32 [ 3, 270, 86942 ] ) ]
            |> expectBytes "2206038E029EA705"
            |> test "a packed list"
        , Encode.message [ ( 5, Encode.repeated Encode.string [ "foo", "bar" ] ) ]
            |> expectBytes "2A03666F6F2A03626172"
            |> test "a non-packed list"
        , Encode.message [ ( 6, Encode.repeated Encode.string [] ) ]
            |> expectBytes ""
            |> test "an empty list"
        ]


expectBytes : String -> Encode.Encoder -> () -> Expectation
expectBytes hex encoder _ =
    Expect.equal (Hex.fromBytes <| Encode.encode encoder) hex
