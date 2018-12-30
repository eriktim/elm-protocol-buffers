module Encode exposing (suite)

import Bytes.Encode
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Hex
import ProtoBuf.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "Should correctly encode"
        [ describe "integers"
            [ Encode.message [ ( 1, Encode.int32 64 ) ]
                |> expectBytes "0840"
                |> test "an int32"
            , Encode.message [ ( 1, Encode.int32 -64 ) ]
                |> expectBytes "08C0FFFFFF0F"
                |> test "a negative int32"
            , Encode.message [ ( 1, Encode.uint32 (2 ^ 32 - 1) ) ]
                |> expectBytes "08FFFFFFFF0F"
                |> test "an uint32"
            , Encode.message [ ( 1, Encode.sint32 64 ) ]
                |> expectBytes "088001"
                |> test "a sint32"
            , Encode.message [ ( 1, Encode.sint32 -64 ) ]
                |> expectBytes "087F"
                |> test "a negative sint32"
            , Encode.message [ ( 1, Encode.fixed32 (2 ^ 32 - 1) ) ]
                |> expectBytes "0DFFFFFFFF"
                |> test "a fixed32"
            , Encode.message [ ( 1, Encode.sfixed32 64 ) ]
                |> expectBytes "0D40000000"
                |> test "a sfixed32"
            , Encode.message [ ( 1, Encode.sfixed32 -64 ) ]
                |> expectBytes "0DC0FFFFFF"
                |> test "a negative sfixed32"
            ]
        , describe "floats"
            [ Encode.message [ ( 1, Encode.double 0.01 ) ]
                |> expectBytes "097B14AE47E17A843F"
                |> test "a double"
            , Encode.message [ ( 1, Encode.float 0.01 ) ]
                |> expectBytes "0D0AD7233C"
                |> test "a float"
            ]
        , describe "strings"
            [ Encode.message [ ( 2, Encode.string "testing" ) ]
                |> expectBytes "120774657374696E67"
                |> test "a string"
            ]
        , describe "booleans"
            [ Encode.message [ ( 1, Encode.bool True ) ]
                |> expectBytes "0801"
                |> test "a boolean"
            ]
        , describe "bytes"
            [ Hex.toBytes "ABCDEF"
                |> Maybe.withDefault (Bytes.Encode.encode (Bytes.Encode.sequence []))
                |> Encode.bytes
                |> Tuple.pair 5
                |> List.singleton
                |> Encode.message
                |> expectBytes "2A03ABCDEF"
                |> test "some bytes"
            ]
        , describe "data structures"
            [ Encode.message [ ( 4, Encode.list Encode.int32 [ 3, 270, 86942 ] ) ]
                |> expectBytes "2206038E029EA705"
                |> test "a packed list"
            , Encode.message [ ( 5, Encode.list Encode.string [ "foo", "bar" ] ) ]
                |> expectBytes "2A03666F6F2A03626172"
                |> test "a non-packed list"
            , Encode.message [ ( 6, Encode.list Encode.string [] ) ]
                |> expectBytes ""
                |> test "an empty list"
            , Encode.message [ ( 7, Encode.dict Encode.int32 Encode.string (Dict.fromList [ ( 1, "foo" ), ( 2, "bar" ) ]) ) ]
                |> expectBytes "3A0708011203666F6F3A0708021203626172"
                |> test "a dict"
            ]
        , describe "protocol buffers"
            [ Encode.message [ ( 2 ^ 29 - 1, Encode.int32 1 ) ]
                |> expectBytes "F8FFFFFF0F01"
                |> test "field numbers"
            , Encode.message [ ( 3, Encode.message [ ( 1, Encode.int32 150 ) ] ) ]
                |> expectBytes "1A03089601"
                |> test "an embedded message"
            ]
        ]


expectBytes : String -> Encode.Encoder -> () -> Expectation
expectBytes hex encoder _ =
    Expect.equal (Hex.fromBytes <| Encode.encode encoder) hex
