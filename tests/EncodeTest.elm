module EncodeTest exposing (suite)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Helper exposing (..)
import Hex
import Protobuf.Codec as Codec
import Protobuf.Message as Message
import Test exposing (..)


suite : Test
suite =
    describe "Should correctly encode"
        [ describe "integers"
            [ test "an int32" <|
                expectBytes (singleField Codec.int32) 64 "0840"
            , test "a negative int32" <|
                expectBytes (singleField Codec.int32) -64 "08C0FFFFFF0F"
            , test "an uint32" <|
                expectBytes (singleField Codec.uint32) (2 ^ 32 - 1) "08FFFFFFFF0F"
            , test "a sint32" <|
                expectBytes (singleField Codec.sint32) 64 "088001"
            , test "a negative sint32" <|
                expectBytes (singleField Codec.sint32) -64 "087F"
            , test "a fixed32" <|
                expectBytes (singleField Codec.fixed32) (2 ^ 32 - 1) "0DFFFFFFFF"
            , test "a sfixed32" <|
                expectBytes (singleField Codec.sfixed32) 64 "0D40000000"
            , test "a negative sfixed32" <|
                expectBytes (singleField Codec.sfixed32) -64 "0DC0FFFFFF"
            ]
        , describe "floats"
            [ test "a double" <|
                expectBytes (singleField Codec.double) 0.01 "097B14AE47E17A843F"
            , test "a float" <|
                expectBytes (singleField Codec.float) 0.01 "0D0AD7233C"
            ]
        , describe "strings"
            [ test "a string" <|
                expectBytes
                    (Codec.builder identity
                        |> Codec.required 2 Codec.string identity
                        |> Codec.build
                    )
                    "testing"
                    "120774657374696E67"
            ]
        , describe "booleans"
            [ test "a boolean" <|
                expectBytes (singleField Codec.bool) True "0801"
            ]
        , describe "bytes"
            [ test "some bytes" <|
                -- FIXME was field 5
                expectBytes
                    (Codec.builder identity
                        |> Codec.required 5 Codec.bytes identity
                        |> Codec.build
                    )
                    (Maybe.withDefault emptyBytes <| Hex.toBytes "ABCDEF")
                    "2A03ABCDEF"
            ]
        , describe "data structures"
            [ test "a packed list" <|
                expectBytes
                    (Codec.builder identity
                        |> Codec.repeated 4 Codec.int32 identity
                        |> Codec.build
                    )
                    [ 3, 270, 86942 ]
                    "2206038E029EA705"
            , test "a non-packed list" <|
                expectBytes
                    (Codec.builder identity
                        |> Codec.repeated 5 Codec.string identity
                        |> Codec.build
                    )
                    [ "foo", "bar" ]
                    "2A03666F6F2A03626172"
            , test "an empty list" <|
                expectBytes
                    (Codec.builder identity
                        |> Codec.repeated 6 Codec.string identity
                        |> Codec.build
                    )
                    []
                    ""
            , test "a dict" <|
                expectBytes
                    (Codec.builder identity
                        |> Codec.map 7 Codec.int32 Codec.string identity
                        |> Codec.build
                    )
                    (Dict.fromList [ ( 1, "foo" ), ( 2, "bar" ) ])
                    "3A0708011203666F6F3A0708021203626172"
            ]
        , describe "protocol buffers"
            [ test "field numbers" <|
                expectBytes
                    (Codec.builder identity
                        |> Codec.required (2 ^ 29 - 1) Codec.int32 identity
                        |> Codec.build
                    )
                    1
                    "F8FFFFFF0F01"
            , test "an embedded message" <|
                expectBytes
                    (Codec.builder identity
                        |> Codec.required 3
                            (Codec.builder identity
                                |> Codec.required 1 Codec.int32 identity
                                |> Codec.build
                            )
                            identity
                        |> Codec.build
                    )
                    (Message.init 150)
                    "1A03089601"
            , test "a default value" <|
                expectBytes
                    (Codec.builder identity
                        |> Codec.field 1 Codec.int32 identity
                        |> Codec.build
                    )
                    0
                    ""
            , test "a non-default value" <|
                expectBytes
                    (Codec.builder identity
                        |> Codec.field 1 Codec.int32 identity
                        |> Codec.build
                    )
                    100
                    "0864"
            ]
        ]


expectBytes : Codec.Codec (Message.Message a) -> a -> String -> () -> Expectation
expectBytes codec value hex =
    \_ -> Expect.equal (Hex.fromBytes <| Codec.encode codec (Message.init value)) hex
