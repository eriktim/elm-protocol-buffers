module Message exposing (suite)

import Bytes
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, float, int, list, maybe, string)
import Hex
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "Should successfully transmit"
        [ describe "all integers"
            [ fuzz int32 "integers" <|
                transmit Encode.int32 Decode.int32
            , fuzz uint32 "unsigned integers" <|
                transmit Encode.uint32 Decode.uint32
            , fuzz int32 "zig-zag encoded integers" <|
                transmit Encode.sint32 Decode.sint32
            , fuzz uint32 "fixed-size unsigned integers" <|
                transmit Encode.fixed32 Decode.fixed32
            , fuzz int32 "fixed-size integers" <|
                transmit Encode.sfixed32 Decode.sfixed32
            ]
        , describe "all floats"
            [ fuzz float "doubles" <|
                transmit Encode.double Decode.double
            , fuzz float32 "floats" <|
                transmit Encode.float Decode.float
            ]
        , describe "all strings"
            [ fuzz string "strings" <|
                transmit Encode.string Decode.string
            ]
        , describe "all booleans"
            [ fuzz bool "booleans" <|
                transmit Encode.bool Decode.bool
            ]
        , describe "all bytes"
            [ fuzz bytes "bytes" <|
                transmit Encode.bytes Decode.bytes
            ]
        , describe "data structures"
            [ fuzz2 fieldNumber (dict int32 string) "dicts" <|
                \num value ->
                    Encode.message [ ( num, Encode.dict Encode.int32 Encode.string value ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message Dict.empty [ Decode.mapped num ( 0, "" ) Decode.int32 Decode.string identity updateSelf ])
                        |> Expect.equal (Just value)
            , fuzz2 fieldNumber enum "custom types without associated data" <|
                \num value ->
                    Encode.message [ ( num, toEnumEncoder value ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message EnumA [ Decode.required num enumDecoder updateSelf ])
                        |> Expect.equal (Just value)
            , fuzz (maybe oneOf) "custom types with associated data" <|
                \value ->
                    Encode.message [ Maybe.withDefault ( 0, Encode.none ) <| Maybe.map toOneOfEncoder value ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message Nothing [ Decode.oneOf oneOfDecoders updateSelf ])
                        |> Expect.equal (Just value)
            ]
        , describe "protocol buffer"
            [ fuzz fieldNumber "field numbers" <|
                \num ->
                    Encode.message [ ( num, Encode.string "field" ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message "" [ Decode.required num Decode.string updateSelf ])
                        |> Expect.equal (Just "field")
            , fuzz string "default values" <|
                \default ->
                    Encode.message []
                        |> Encode.encode
                        |> Decode.decode (Decode.message default [ Decode.optional 1 Decode.string updateSelf ])
                        |> Expect.equal (Just default)
            , fuzz message "messages" <|
                \value ->
                    toMessageEncoder value
                        |> Encode.encode
                        |> Decode.decode messageDecoder
                        |> Expect.equal (Just value)
            ]
        ]



-- FUZZERS


fieldNumber : Fuzzer Int
fieldNumber =
    Fuzz.intRange 1 (2 ^ 29 - 1)


int32 : Fuzzer Int
int32 =
    Fuzz.intRange (-2 ^ 31) (2 ^ 31 - 1)


uint32 : Fuzzer Int
uint32 =
    -- max `Fuzz.int` is only `2 ^ 31 - 1`
    Fuzz.floatRange 0 (2 ^ 32 - 1)
        |> Fuzz.map round


float32 : Fuzzer Float
float32 =
    Fuzz.map
        (\v ->
            Bytes.Encode.encode (Bytes.Encode.float32 Bytes.LE v)
                |> Bytes.Decode.decode (Bytes.Decode.float32 Bytes.LE)
                |> Maybe.withDefault v
        )
        float


bytes : Fuzzer Bytes.Bytes
bytes =
    Fuzz.map (Bytes.Encode.encode << Bytes.Encode.sequence << List.map (Bytes.Encode.signedInt32 Bytes.LE)) (list int32)


dict : Fuzzer comparable -> Fuzzer v -> Fuzzer (Dict comparable v)
dict k v =
    Fuzz.map2 (\ks vs -> Dict.fromList <| List.map2 Tuple.pair ks vs) (list k) (list v)



-- ENUM


type Enum
    = EnumA
    | EnumB
    | EnumC
    | Unrecognized Int


enum : Fuzzer Enum
enum =
    Fuzz.map enumType (Fuzz.intRange 0 3)


enumType : Int -> Enum
enumType value =
    case value of
        0 ->
            EnumA

        1 ->
            EnumB

        2 ->
            EnumC

        v ->
            Unrecognized v


toEnumEncoder : Enum -> Encode.Encoder
toEnumEncoder value =
    Encode.int32 <|
        case value of
            EnumA ->
                0

            EnumB ->
                1

            EnumC ->
                2

            Unrecognized v ->
                v


enumDecoder : Decode.Decoder Enum
enumDecoder =
    Decode.map enumType Decode.int32



-- ONE OF


type OneOf
    = String String
    | Int32 Int
    | Double Float


oneOf : Fuzzer OneOf
oneOf =
    Fuzz.oneOf
        [ Fuzz.map String string
        , Fuzz.map Int32 int32
        , Fuzz.map Double float
        ]


toOneOfEncoder : OneOf -> ( Int, Encode.Encoder )
toOneOfEncoder value =
    case value of
        String v ->
            ( 1, Encode.string v )

        Int32 v ->
            ( 2, Encode.int32 v )

        Double v ->
            ( 3, Encode.double v )


oneOfDecoders : List ( Int, Decode.Decoder OneOf )
oneOfDecoders =
    [ ( 1, Decode.map String Decode.string )
    , ( 2, Decode.map Int32 Decode.int32 )
    , ( 3, Decode.map Double Decode.double )
    ]



-- MESSAGE


type alias Message =
    { string : String
    , double : Float
    , message : Maybe ListMessage
    }


type alias ListMessage =
    { nonPackable : List String
    , packed : List Int
    , notPacked : List Int
    }


message : Fuzzer Message
message =
    Fuzz.map3 Message string float (maybe listMessage)


listMessage : Fuzzer ListMessage
listMessage =
    Fuzz.map3 ListMessage (list string) (list int32) (list int32)


toMessageEncoder : Message -> Encode.Encoder
toMessageEncoder value =
    Encode.message
        [ ( 1, Encode.string value.string )
        , ( 2, Encode.double value.double )
        , ( 3, Maybe.withDefault Encode.none <| Maybe.map toListMessageEncoder value.message )
        ]


toListMessageEncoder : ListMessage -> Encode.Encoder
toListMessageEncoder value =
    Encode.message <|
        [ ( 1, Encode.list Encode.string value.nonPackable )
        , ( 2, Encode.list Encode.int32 value.packed )
        ]
            ++ List.map (Tuple.pair 3 << Encode.int32) value.notPacked


messageDecoder : Decode.Decoder Message
messageDecoder =
    Decode.message (Message "" 0 Nothing)
        [ Decode.optional 1 Decode.string (\value model -> { model | string = value })
        , Decode.optional 2 Decode.double (\value model -> { model | double = value })
        , Decode.optional 3 (Decode.map Just listMessageDecoder) (\value model -> { model | message = value })
        ]


listMessageDecoder : Decode.Decoder ListMessage
listMessageDecoder =
    Decode.message (ListMessage [] [] [])
        [ Decode.repeated 1 Decode.string .nonPackable (\value model -> { model | nonPackable = value })
        , Decode.repeated 2 Decode.int32 .packed (\value model -> { model | packed = value })
        , Decode.repeated 3 Decode.int32 .notPacked (\value model -> { model | notPacked = value })
        ]



-- TRANSMIT


transmit : (a -> Encode.Encoder) -> Decode.Decoder a -> a -> Expectation
transmit encoder decoder value =
    Encode.encode (encoder value)
        |> Decode.decode decoder
        |> Expect.equal (Just value)


updateSelf : a -> a -> a
updateSelf value _ =
    value
