module EncodeTest exposing (suite)

import Bytes.Encode
import Dict
import Expect exposing (Expectation)
import Hex
import Int64 exposing (Int64)
import Protobuf.Encode as Encode
import Test exposing (..)
import UInt64 exposing (UInt64)


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
            , Encode.message
                [ ( 1
                  , Encode.int64
                        (Int64.fromInt 2147483647
                            |> int64Times 3
                        )
                  )
                ]
                |> expectBytes "08FDFFFFFF17"
                |> test "an int64"
            , Encode.message
                [ ( 1
                  , Encode.int64
                        (Int64.fromInt -12345)
                  )
                ]
                |> expectBytes "08C79FFFFFFFFFFFFFFF01"
                |> test "a negative int64"
            , Encode.message
                [ ( 1, Encode.uint64 (UInt64.fromInt 2345678) ) ]
                |> expectBytes "08CE958F01"
                |> test "an uint64"
            , Encode.message
                [ ( 1, Encode.uint64 UInt64.maxValue ) ]
                |> expectBytes "08FFFFFFFFFFFFFFFFFF01"
                |> test "a large uint64"
            , Encode.message
                [ ( 1
                  , Encode.sint64
                        (Int64.fromInt 2147483647
                            |> int64Times 3
                        )
                  )
                ]
                |> expectBytes "08FAFFFFFF2F"
                |> test "a sint64"
            , Encode.message
                [ ( 1
                  , Encode.sint64
                        (Int64.fromInt -12345)
                  )
                ]
                |> expectBytes "08F1C001"
                |> test "a negative sint64"
            , Encode.message
                [ ( 1
                  , Encode.sfixed64
                        (Int64.fromInt 2147483647
                            |> int64Times 3
                        )
                  )
                ]
                |> expectBytes "09FDFFFF7F01000000"
                |> test "a sfixed64"
            , Encode.message
                [ ( 1
                  , Encode.sfixed64
                        (Int64.fromInt -2147483647
                            |> int64Times 3
                        )
                  )
                ]
                |> expectBytes "0903000080FEFFFFFF"
                |> test "a negative sfixed64"
            , Encode.message
                [ ( 1
                  , Encode.fixed64
                        (UInt64.fromInt 2147483647
                            |> uint64Times 3
                        )
                  )
                ]
                |> expectBytes "09FDFFFF7F01000000"
                |> test "a fixed64"
            , Encode.message
                [ ( 1
                  , Encode.fixed64
                        (UInt64.fromInt32s 0x11223344 0xAABBCCDD)
                  )
                ]
                |> expectBytes "09DDCCBBAA44332211"
                |> test "a large fixed64"
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


int64Times : Int -> Int64 -> Int64
int64Times n i =
    if n == 0 then
        Int64.fromInt 0

    else
        Int64.add i <| int64Times (n - 1) i


uint64Times : Int -> UInt64 -> UInt64
uint64Times n i =
    if n == 0 then
        UInt64.fromInt 0

    else
        UInt64.add i <| uint64Times (n - 1) i
