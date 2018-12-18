module Message exposing (suite)

import Bytes
import Bytes.Decode
import Bytes.Encode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, float, int, list, maybe, string)
import Hex
import ProtoBuf.Decode as Decode
import ProtoBuf.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "Should successfully transmit"
        [ fuzz fieldNumber "field numbers" <|
            \num ->
                Encode.message [ ( num, Encode.int32 1 ) ]
                    |> Encode.encode
                    |> Decode.decode (Decode.message identity |> Decode.field num Decode.int32)
                    |> Expect.equal (Just 1)
        , fuzz float "a double" <|
            transmit Encode.double Decode.double
        , fuzz float32 "a float" <|
            transmit Encode.float Decode.float
        , fuzz int32 "an int32" <|
            transmit Encode.int32 Decode.int32
        , fuzz uint32 "an uint32" <|
            transmit Encode.uint32 Decode.uint32
        , fuzz int32 "a sint32" <|
            transmit Encode.sint32 Decode.sint32
        , fuzz uint32 "a fixed32" <|
            transmit Encode.fixed32 Decode.fixed32
        , fuzz int32 "an sfixed32" <|
            transmit Encode.sfixed32 Decode.sfixed32
        , fuzz bool "a boolean" <|
            transmit Encode.bool Decode.bool
        , fuzz string "a string" <|
            transmit Encode.string Decode.string
        , fuzz (list int32) "a packed list" <|
            transmit (Encode.repeated Encode.int32) (Decode.repeated Decode.int32)
        , fuzz (list string) "a non-packed list" <|
            transmit (Encode.repeated Encode.string) (Decode.repeated Decode.string)
        , fuzz message "a message" <|
            \value ->
                messageEncoder value
                    |> Encode.encode
                    |> Decode.decode messageDecoder
                    |> Expect.equal (Just value)
        ]



-- HELPERS


float32 : Fuzzer Float
float32 =
    Fuzz.map
        (\v ->
            Bytes.Encode.encode (Bytes.Encode.float32 Bytes.LE v)
                |> Bytes.Decode.decode (Bytes.Decode.float32 Bytes.LE)
                |> Maybe.withDefault v
        )
        float


int32 : Fuzzer Int
int32 =
    Fuzz.intRange (-2 ^ 31) (2 ^ 31 - 1)


uint32 : Fuzzer Int
uint32 =
    Fuzz.intRange 0 (2 ^ 32 - 1)


fieldNumber : Fuzzer Int
fieldNumber =
    Fuzz.intRange 1 (2 ^ 29 - 1)


type alias Message =
    { string : String
    , double : Float
    , message : Maybe ListMessage
    }


type alias ListMessage =
    { strings : List String
    , int32s : List Int
    }


message : Fuzzer Message
message =
    Fuzz.map3 Message string float (maybe listMessage)


listMessage : Fuzzer ListMessage
listMessage =
    Fuzz.map2 ListMessage (list string) (list int32)


messageEncoder : Message -> Encode.Encoder
messageEncoder value =
    Encode.message
        [ ( 1, Encode.string value.string )
        , ( 2, Encode.double value.double )
        , ( 3, Encode.embedded listMessageEncoder value.message )
        ]


listMessageEncoder : ListMessage -> Encode.Encoder
listMessageEncoder value =
    Encode.message
        [ ( 1, Encode.repeated Encode.string value.strings )
        , ( 2, Encode.repeated Encode.int32 value.int32s )
        ]


messageDecoder : Decode.Decoder Message
messageDecoder =
    Decode.message Message
        |> Decode.field 1 Decode.string
        |> Decode.field 2 Decode.double
        |> Decode.field 3 (Decode.embedded listMessageDecoder)


listMessageDecoder : Decode.Decoder ListMessage
listMessageDecoder =
    Decode.message ListMessage
        |> Decode.field 1 (Decode.repeated Decode.string)
        |> Decode.field 2 (Decode.repeated Decode.int32)


transmit : (a -> Encode.FieldEncoder) -> Decode.FieldDecoder a -> a -> Expectation
transmit encoder decoder value =
    Encode.message [ ( 100, encoder value ) ]
        |> Encode.encode
        |> Decode.decode (Decode.message identity |> Decode.field 100 decoder)
        |> Expect.equal (Just value)
