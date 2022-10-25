module EncodeTest exposing (suite)

import Bytes.Encode
import Dict
import Expect exposing (Expectation)
import Hex
import Protobuf.Types.Int64 as Int64
import Protobuf.Encode as Encode
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
            , Encode.message
                [ ( 1
                  , Encode.int64 <|
                        Int64.fromInt32s { lower = 2147483645, upper = 1 }
                  )
                ]
                |> expectBytes "08FDFFFFFF17"
                |> test "an int64"
            , Encode.message
                [ ( 1
                  , Encode.int64 <| Int64.fromInt32s { upper = 4294967295, lower = 4294954951 }
                  )
                ]
                |> expectBytes "08C79FFFFFFFFFFFFFFF01"
                |> test "a negative int64"
            , Encode.message
                [ ( 1, Encode.uint64 <| Int64.fromInt32s { upper = 0, lower = 2345678 } ) ]
                |> expectBytes "08CE958F01"
                |> test "an uint64"
            , Encode.message
                [ ( 1, Encode.uint64 <| Int64.fromInt32s { upper = 0xFFFFFFFF, lower = 0xFFFFFFFF } ) ]
                |> expectBytes "08FFFFFFFFFFFFFFFFFF01"
                |> test "a large uint64"
            , Encode.message
                [ ( 1
                  , Encode.sint64 <|
                        Int64.fromInt32s
                            { lower = 2147483645, upper = 1 }
                  )
                ]
                |> expectBytes "08FAFFFFFF2F"
                |> test "a sint64"
            , Encode.message
                [ ( 1
                  , Encode.sint64 <|
                        Int64.fromInt32s { upper = -1, lower = -12345 }
                  )
                ]
                |> expectBytes "08F1C001"
                |> test "a negative sint64"
            , Encode.message
                [ ( 1
                  , Encode.sfixed64 <|
                        Int64.fromInt32s { lower = 2147483645, upper = 1 }
                  )
                ]
                |> expectBytes "09FDFFFF7F01000000"
                |> test "a sfixed64"
            , Encode.message
                [ ( 1
                  , Encode.sfixed64 <|
                        Int64.fromInt32s { lower = -2147483645, upper = -1 }
                  )
                ]
                |> expectBytes "0903000080FFFFFFFF"
                |> test "a negative sfixed64"
            , Encode.message
                [ ( 1
                  , Encode.fixed64 <|
                        Int64.fromInt32s { lower = 2147483645, upper = 1 }
                  )
                ]
                |> expectBytes "09FDFFFF7F01000000"
                |> test "a fixed64"
            , Encode.message
                [ ( 1
                  , Encode.fixed64 <|
                        Int64.fromInt32s { lower = 0x11223344, upper = 0xAABBCCDD }
                  )
                ]
                |> expectBytes "0944332211DDCCBBAA"
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
