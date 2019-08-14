module CodecTest exposing (suite)

import Bytes
import Bytes.Decode
import Bytes.Encode
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, float, int, list, maybe, string)
import Helper exposing (..)
import Hex
import Protobuf.Codec as Codec
import Protobuf.Message as Message
import Test exposing (..)


suite : Test
suite =
    describe "Should successfully transmit"
        [ describe "all integers"
            [ fuzz int32 "integers" <|
                expectSymmetric (singleField Codec.int32)
            , fuzz uint32 "unsigned integers" <|
                expectSymmetric (singleField Codec.uint32)
            , fuzz int32 "zig-zag encoded integers" <|
                expectSymmetric (singleField Codec.sint32)
            , fuzz uint32 "fixed-size unsigned integers" <|
                expectSymmetric (singleField Codec.fixed32)
            , fuzz int32 "fixed-size integers" <|
                expectSymmetric (singleField Codec.sfixed32)
            ]
        , describe "all floats"
            [ fuzz float "doubles" <|
                expectSymmetric (singleField Codec.double)
            , fuzz float32 "floats" <|
                expectSymmetric (singleField Codec.float)
            ]
        , describe "all strings"
            [ fuzz string "strings" <|
                expectSymmetric (singleField Codec.string)
            ]
        , describe "all booleans"
            [ fuzz bool "booleans" <|
                expectSymmetric (singleField Codec.bool)
            ]
        , describe "all bytes"
            [ fuzz bytes "bytes" <|
                expectSymmetric (singleField Codec.bytes)
            ]
        , describe "data structures"
            [ fuzz (dict int32 string) "dictionaries" <|
                expectSymmetric mapCodec
            , fuzz enum "custom types without associated data" <|
                expectSymmetric (singleField enumCodec)
            , fuzz (maybe oneOf) "custom types with associated data" <|
                expectSymmetric
                    (Codec.builder identity
                        |> Codec.oneof oneofCodec identity
                        |> Codec.build
                    )
            ]
        , describe "protocol buffers"
            [ fuzz fieldNumber "field numbers" <|
                \num -> expectSymmetric (fieldNumberCodecBuilder num) "value"
            , fuzz string "default values" <|
                \defaultValue ->
                    Codec.decode (defaultValueCodecBuilder defaultValue) emptyBytes
                        |> Maybe.map (Message.view identity)
                        |> Expect.equal (Just defaultValue)
            , fuzz data "messages" <|
                expectSymmetric dataCodec
            , fuzz3 string string int32 "forward and backward compatibility" <|
                \firstName lastName age ->
                    let
                        fullName =
                            firstName ++ " " ++ lastName

                        bytes1 =
                            Message.init (Person1 fullName age)
                                |> Codec.encode person1Codec

                        person2 =
                            Codec.decode person2Codec bytes1
                                |> Maybe.withDefault (Message.init <| Person2 "" "" 0)
                                |> Message.set (\first person -> { person | firstName = first }) firstName
                                |> Message.set (\last person -> { person | lastName = last }) lastName

                        person1 =
                            Codec.encode person2Codec person2
                                |> Codec.decode person1Codec
                                |> Maybe.withDefault (Message.init <| Person1 "" 0)
                    in
                    Expect.all
                        [ Expect.equal fullName << Message.view .name << Tuple.first
                        , Expect.equal age << Message.view .age << Tuple.first
                        , Expect.equal firstName << Message.view .firstName << Tuple.second
                        , Expect.equal lastName << Message.view .lastName << Tuple.second
                        , Expect.equal age << Message.view .age << Tuple.second
                        ]
                        ( person1, person2 )
            ]
        ]



-- DATA STRUCTURES


mapCodec : Codec.Codec (Message.Message (Dict.Dict Int String))
mapCodec =
    Codec.builder identity
        |> Codec.map 1 Codec.int32 Codec.string identity
        |> Codec.build


type Enum
    = EnumA
    | EnumB
    | EnumC


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

        _ ->
            EnumA


enumCodec : Codec.Codec Enum
enumCodec =
    Codec.invmap
        enumType
        (\model ->
            case model of
                EnumA ->
                    0

                EnumB ->
                    1

                EnumC ->
                    2
        )
        Codec.int32


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


oneofCodec : Codec.OneofCodec OneOf
oneofCodec =
    Codec.oneofBuilder
        (\stringEncoder intEncoder doubleEncoder model ->
            case model of
                String v ->
                    stringEncoder v

                Int32 v ->
                    intEncoder v

                Double v ->
                    doubleEncoder v
        )
        |> Codec.oneofField 1 String Codec.string
        |> Codec.oneofField 2 Int32 Codec.int32
        |> Codec.oneofField 3 Double Codec.double
        |> Codec.buildOneof



-- PROTOBUF


fieldNumberCodecBuilder : Int -> Codec.Codec (Message.Message String)
fieldNumberCodecBuilder num =
    Codec.builder identity
        |> Codec.field num Codec.string identity
        |> Codec.build


defaultValueCodecBuilder : String -> Codec.Codec (Message.Message String)
defaultValueCodecBuilder defaultValue =
    Codec.builder identity
        |> Codec.optional 1 Codec.string identity (Just defaultValue)
        |> Codec.build


type alias Data =
    { string : String
    , double : Float
    , list : Maybe (Message.Message ListData)
    , recursive : Maybe (Message.Message RecursiveData)
    }


type alias ListData =
    { nonPackable : List String
    , packed : List Int
    , notPacked : List Int
    }


type alias RecursiveData =
    { maybe : Maybe RecursiveDataMessage
    , list : List RecursiveDataMessage
    , dict : Dict.Dict String RecursiveDataMessage
    }


type RecursiveDataMessage
    = RecursiveDataMessage (Message.Message RecursiveData)


data : Fuzzer Data
data =
    Fuzz.map4 Data
        string
        float
        (maybe <| Fuzz.map Message.init listData)
        (maybe <| Fuzz.map Message.init recursiveData)


listData : Fuzzer ListData
listData =
    Fuzz.map3 ListData (list string) (list int32) (list int32)


recursiveData : Fuzzer RecursiveData
recursiveData =
    Fuzz.map3 RecursiveData
        (Fuzz.map
            (\withMaybe ->
                if withMaybe then
                    Just (RecursiveDataMessage emptyRecursiveData)

                else
                    Nothing
            )
            bool
        )
        (Fuzz.map
            (\n ->
                List.map RecursiveDataMessage (List.repeat n emptyRecursiveData)
            )
            (Fuzz.intRange 0 100)
        )
        (Fuzz.map
            (\keys ->
                List.map (\key -> ( key, RecursiveDataMessage emptyRecursiveData )) keys
                    |> Dict.fromList
            )
            (list string)
        )


emptyRecursiveData : Message.Message RecursiveData
emptyRecursiveData =
    Message.init <| RecursiveData Nothing [] Dict.empty


dataCodec : Codec.Codec (Message.Message Data)
dataCodec =
    Codec.builder Data
        |> Codec.field 1 Codec.string .string
        |> Codec.field 2 Codec.double .double
        |> Codec.field 3 (Codec.maybe listDataCodec) .list
        |> Codec.field 4 (Codec.maybe recursiveDataCodec) .recursive
        |> Codec.build


listDataCodec : Codec.Codec (Message.Message ListData)
listDataCodec =
    Codec.builder ListData
        |> Codec.repeated 1 Codec.string .nonPackable
        |> Codec.repeated 2 Codec.int32 .packed
        |> Codec.repeatedInefficiently 3 Codec.int32 .notPacked
        |> Codec.build


recursiveDataCodec : Codec.Codec (Message.Message RecursiveData)
recursiveDataCodec =
    Codec.builder RecursiveData
        |> Codec.field 1 (Codec.maybe recursiveDataMessageCodec) .maybe
        |> Codec.repeated 2 recursiveDataMessageCodec .list
        |> Codec.map 3 Codec.string recursiveDataMessageCodec .dict
        |> Codec.build


recursiveDataMessageCodec : Codec.Codec RecursiveDataMessage
recursiveDataMessageCodec =
    Codec.lazy (\_ -> Codec.invmap RecursiveDataMessage (\(RecursiveDataMessage rd) -> rd) recursiveDataCodec)


type alias Person1 =
    { name : String
    , age : Int
    }


type alias Person2 =
    { firstName : String
    , lastName : String
    , age : Int
    }


person1Codec : Codec.Codec (Message.Message Person1)
person1Codec =
    Codec.builder Person1
        |> Codec.field 1 Codec.string .name
        |> Codec.field 2 Codec.int32 .age
        |> Codec.build


person2Codec : Codec.Codec (Message.Message Person2)
person2Codec =
    Codec.builder Person2
        |> Codec.field 3 Codec.string .firstName
        |> Codec.field 4 Codec.string .lastName
        |> Codec.field 2 Codec.int32 .age
        |> Codec.build
