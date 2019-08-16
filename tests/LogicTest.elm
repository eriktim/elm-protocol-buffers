module LogicTest exposing (suite)

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
    describe "Should correctly decode"
        [ describe "integers"
            [ test "an int32" <|
                expectValue (messageCodec Codec.int32) "" 0
            ]
        , describe "floats"
            [ test "a double" <|
                expectValue (messageCodec Codec.double) "" 0
            ]
        , describe "strings"
            [ test "a string" <|
                expectValue (messageCodec Codec.string) "1064" ""
            , test "an unknown field preserving string" <|
                expectBytes (messageCodec Codec.string) "1064" "1064"
            ]
        , describe "booleans"
            [ test "a boolean" <|
                expectValue (messageCodec Codec.bool) "" False
            ]
        , describe "enumerations"
            [ test "an enum" <|
                expectValue (messageCodec enumCodec) "" (Just EnumA)
            , test "an unknown enum" <|
                expectValue (messageCodec enumCodec) "0863" Nothing
            , test "an unknown field preserving enum" <|
                expectBytes (messageCodec enumCodec) "0863" "0863"
            , test "an unsafe enum" <|
                expectValue unsafeEnumCodec "" EnumA
            , test "an unknown unsafe enum" <|
                expectValue unsafeEnumCodec "0863" EnumA
            , test "an unknown field overwriting unsafe enum" <|
                expectBytes unsafeEnumCodec "0863" "08630800"
            ]
        ]


messageCodec : Codec.Codec a -> Codec.Codec (Message.Message a)
messageCodec codec =
    Codec.builder identity
        |> Codec.field 1 codec identity
        |> Codec.build


expectValue : Codec.Codec (Message.Message a) -> String -> a -> () -> Expectation
expectValue codec hex value =
    \_ ->
        Hex.toBytes hex
            |> Maybe.andThen (Codec.decode codec)
            |> Maybe.map (Message.view identity)
            |> Expect.equal (Just value)


expectBytes : Codec.Codec (Message.Message a) -> String -> String -> () -> Expectation
expectBytes codec hex expected =
    \_ ->
        Hex.toBytes hex
            |> Maybe.andThen (Codec.decode codec)
            |> Maybe.map (Hex.fromBytes << Codec.encode codec)
            |> Expect.equal (Just expected)



-- ENUM


type Enum
    = EnumA
    | EnumB
    | EnumC


enumCodec : Codec.Codec (Maybe Enum)
enumCodec =
    Codec.enum
        [ ( 0, EnumA ), ( 1, EnumB ), ( 2, EnumC ) ]


unsafeEnumCodec : Codec.Codec (Message.Message Enum)
unsafeEnumCodec =
    Codec.builder identity
        |> Codec.optional 1
            (Codec.enumUnsafe EnumA
                [ ( 0, EnumA ), ( 1, EnumB ), ( 2, EnumC ) ]
            )
            identity
            Nothing
        |> Codec.build
