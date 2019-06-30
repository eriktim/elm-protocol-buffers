module Google.Protobuf exposing
    ( Any
    , Api, Method, Mixin
    , Duration
    , Empty
    , FieldMask
    , SourceContext
    , Struct, StructFields(..), Value, ValueKind(..), KindType(..), NullValue(..), ListValue, ListValueValues(..)
    , Timestamp
    , Type, Field, Kind(..), Cardinality(..), Enum, EnumValue, Option, Syntax(..)
    , DoubleValue, FloatValue, Int32Value, UInt32Value, BoolValue, StringValue, BytesValue
    , anyDecoder
    , apiDecoder
    , methodDecoder
    , mixinDecoder
    , durationDecoder
    , emptyDecoder
    , fieldMaskDecoder
    , sourceContextDecoder
    , structDecoder, valueDecoder, listValueDecoder
    , timestampDecoder
    , typeDecoder, fieldDecoder, kindDecoder, cardinalityDecoder, enumDecoder, enumValueDecoder, optionDecoder, syntaxDecoder
    , doubleValueDecoder, floatValueDecoder, int32ValueDecoder, uInt32ValueDecoder, boolValueDecoder, stringValueDecoder, bytesValueDecoder
    , toAnyEncoder
    , toApiEncoder
    , toMethodEncoder
    , toMixinEncoder
    , toDurationEncoder
    , toEmptyEncoder
    , toFieldMaskEncoder
    , toSourceContextEncoder
    , toStructEncoder, toValueEncoder, toListValueEncoder
    , toTimestampEncoder
    , toTypeEncoder, toFieldEncoder, toKindEncoder, toCardinalityEncoder, toEnumEncoder, toEnumValueEncoder, toOptionEncoder, toSyntaxEncoder
    , toDoubleValueEncoder, toFloatValueEncoder, toInt32ValueEncoder, toUInt32ValueEncoder, toBoolValueEncoder, toStringValueEncoder, toBytesValueEncoder
    )

{-| This module contains the **Well-Known Types** of Protocol Buffers.
For the official and complete documentation, see <https://developers.google.com/protocol-buffers/docs/reference/google.protobuf>.


# Models


## Any

@docs Any


## Api

@docs Api, Method, Mixin


## Duration

@docs Duration


## Empty

@docs Empty


## FieldMask

@docs FieldMask


## SourceContext

@docs SourceContext


## Struct

@docs Struct, StructFields, Value, ValueKind, KindType, NullValue, ListValue, ListValueValues


## Timestamp

@docs Timestamp


## Type

@docs Type, Field, Kind, Cardinality, Enum, EnumValue, Option, Syntax


## Wrappers

`Int64Value` and `UInt64Value` are not provided here as explained in the known limitations.

@docs DoubleValue, FloatValue, Int32Value, UInt32Value, BoolValue, StringValue, BytesValue


# Decoders

The package exposes decoders for all top-level well-known types.

@docs anyDecoder
@docs apiDecoder
@docs methodDecoder
@docs mixinDecoder
@docs durationDecoder
@docs emptyDecoder
@docs fieldMaskDecoder
@docs sourceContextDecoder
@docs structDecoder, valueDecoder, listValueDecoder
@docs timestampDecoder
@docs typeDecoder, fieldDecoder, kindDecoder, cardinalityDecoder, enumDecoder, enumValueDecoder, optionDecoder, syntaxDecoder
@docs doubleValueDecoder, floatValueDecoder, int32ValueDecoder, uInt32ValueDecoder, boolValueDecoder, stringValueDecoder, bytesValueDecoder


# Encoders

The package exposes encoders for all top-level well-known types.

@docs toAnyEncoder
@docs toApiEncoder
@docs toMethodEncoder
@docs toMixinEncoder
@docs toDurationEncoder
@docs toEmptyEncoder
@docs toFieldMaskEncoder
@docs toSourceContextEncoder
@docs toStructEncoder, toValueEncoder, toListValueEncoder
@docs toTimestampEncoder
@docs toTypeEncoder, toFieldEncoder, toKindEncoder, toCardinalityEncoder, toEnumEncoder, toEnumValueEncoder, toOptionEncoder, toSyntaxEncoder
@docs toDoubleValueEncoder, toFloatValueEncoder, toInt32ValueEncoder, toUInt32ValueEncoder, toBoolValueEncoder, toStringValueEncoder, toBytesValueEncoder

-}

import Bytes
import Dict
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode



-- MODEL


{-| The syntax in which a protocol buffer element is defined.
-}
type Syntax
    = SyntaxProto2
    | SyntaxProto3
    | SyntaxUnrecognized_ Int


{-| Basic field types.
-}
type Kind
    = TypeUnknown
    | TypeDouble
    | TypeFloat
    | TypeInt64
    | TypeUint64
    | TypeInt32
    | TypeFixed64
    | TypeFixed32
    | TypeBool
    | TypeString
    | TypeGroup
    | TypeMessage
    | TypeBytes
    | TypeUint32
    | TypeEnum
    | TypeSfixed32
    | TypeSfixed64
    | TypeSint32
    | TypeSint64
    | KindUnrecognized_ Int


{-| Whether a field is optional, required, or repeated.
-}
type Cardinality
    = CardinalityUnknown
    | CardinalityOptional
    | CardinalityRequired
    | CardinalityRepeated
    | CardinalityUnrecognized_ Int


{-| `NullValue` is a singleton enumeration to represent the null value for the
`Value` type union.
-}
type NullValue
    = NullValue
    | NullValueUnrecognized_ Int


{-| `Any` contains an arbitrary serialized protocol buffer message along with a
URL that describes the type of the serialized message.
-}
type alias Any =
    { typeUrl : String
    , value : Bytes.Bytes
    }


{-| `SourceContext` represents information about the source of a
protobuf element, like the file in which it is defined.
-}
type alias SourceContext =
    { fileName : String
    }


{-| A protocol buffer message type.
-}
type alias Type =
    { name : String
    , fields : List Field
    , oneofs : List String
    , options : List Option
    , sourceContext : Maybe SourceContext
    , syntax : Syntax
    }


{-| A single field of a message type.
-}
type alias Field =
    { kind : Kind
    , cardinality : Cardinality
    , number : Int
    , name : String
    , typeUrl : String
    , oneofIndex : Int
    , packed : Bool
    , options : List Option
    , jsonName : String
    , defaultValue : String
    }


{-| Enum type definition.
-}
type alias Enum =
    { name : String
    , enumvalue : List EnumValue
    , options : List Option
    , sourceContext : Maybe SourceContext
    , syntax : Syntax
    }


{-| Enum value definition.
-}
type alias EnumValue =
    { name : String
    , number : Int
    , options : List Option
    }


{-| A protocol buffer option, which can be attached to a message, field,
enumeration, etc.
-}
type alias Option =
    { name : String
    , value : Maybe Any
    }


{-| Api is a light-weight descriptor for an API Interface.
-}
type alias Api =
    { name : String
    , methods : List Method
    , options : List Option
    , version : String
    , sourceContext : Maybe SourceContext
    , mixins : List Mixin
    , syntax : Syntax
    }


{-| Method represents a method of an API interface.
-}
type alias Method =
    { name : String
    , requestTypeUrl : String
    , requestStreaming : Bool
    , responseTypeUrl : String
    , responseStreaming : Bool
    , options : List Option
    , syntax : Syntax
    }


{-| Declares an API Interface to be included in this interface.
-}
type alias Mixin =
    { name : String
    , root : String
    }


{-| A Duration represents a signed, fixed-length span of time represented
as a count of seconds and fractions of seconds at nanosecond
resolution. It is independent of any calendar and concepts like "day"
or "month". It is related to Timestamp in that the difference between
two Timestamp values is a Duration and it can be added or subtracted
from a Timestamp.

**Warning**: Range is only +/-68 years instead of the specified +/-10,000 as
64 bit integers are currently not supported.

-}
type alias Duration =
    { seconds : Int
    , nanos : Int
    }


{-| A generic empty message that you can re-use to avoid defining duplicated
empty messages in your APIs. A typical example is to use it as the request
or the response type of an API method.
-}
type alias Empty =
    {}


{-| `FieldMask` represents a set of symbolic field paths.
-}
type alias FieldMask =
    { paths : List String
    }


{-| Wrapper type to work with [recursive aliases](https://elm-lang.org/0.19.0/recursive-alias).
-}
type StructFields
    = StructFields (Dict.Dict String (Maybe Value))


{-| `Struct` represents a structured data value, consisting of fields
which map to dynamically typed values.
-}
type alias Struct =
    { fields : StructFields
    }


{-| Wrapper type to work with [recursive aliases](https://elm-lang.org/0.19.0/recursive-alias).
-}
type ValueKind
    = ValueKind (Maybe KindType)


{-| The kind of value.
-}
type KindType
    = KindNullValue NullValue
    | KindNumberValue Float
    | KindStringValue String
    | KindBoolValue Bool
    | KindStructValue Struct
    | KindListValue ListValue


{-| `Value` represents a dynamically typed value which can be either
null, a number, a string, a boolean, a recursive struct value, or a
list of values. A producer of value is expected to set one of that
variants, absence of any variant indicates an error.
-}
type alias Value =
    { kind : ValueKind
    }


{-| Wrapper type to work with [recursive aliases](https://elm-lang.org/0.19.0/recursive-alias).
-}
type ListValueValues
    = ListValueValues (List Value)


{-| `ListValue` is a wrapper around a repeated field of values.
-}
type alias ListValue =
    { values : ListValueValues
    }


{-| A Timestamp represents a point in time independent of any time zone or local
calendar, encoded as a count of seconds and fractions of seconds at
nanosecond resolution. The count is relative to an epoch at UTC midnight on
January 1, 1970, in the proleptic Gregorian calendar which extends the
Gregorian calendar backwards to year one.

All minutes are 60 seconds long. Leap seconds are "smeared" so that no leap
second table is needed for interpretation, using a [24-hour linear
smear](https://developers.google.com/time/smear).

**Warning**: Range is only from 1901-12-13T20:45:52Z to 2038-01-19T03:14:07Z
instead of the specified 0001-01-01T00:00:00Z to 9999-12-31T23:59:59.999999999Z
as 64 bit integers are currently not supported.

-}
type alias Timestamp =
    { seconds : Int
    , nanos : Int
    }


{-| Wrapper message for `double`.
-}
type alias DoubleValue =
    { value : Float
    }


{-| Wrapper message for `float`.
-}
type alias FloatValue =
    { value : Float
    }


{-| Wrapper message for `int32`.
-}
type alias Int32Value =
    { value : Int
    }


{-| Wrapper message for `uint32`.
-}
type alias UInt32Value =
    { value : Int
    }


{-| Wrapper message for `bool`.
-}
type alias BoolValue =
    { value : Bool
    }


{-| Wrapper message for `string`.
-}
type alias StringValue =
    { value : String
    }


{-| Wrapper message for `bytes`.
-}
type alias BytesValue =
    { value : Bytes.Bytes
    }



-- DECODER


{-| -}
syntaxDecoder : Decode.Decoder Syntax
syntaxDecoder =
    Decode.int32
        |> Decode.map
            (\value ->
                case value of
                    0 ->
                        SyntaxProto2

                    1 ->
                        SyntaxProto3

                    v ->
                        SyntaxUnrecognized_ v
            )


{-| -}
kindDecoder : Decode.Decoder Kind
kindDecoder =
    Decode.int32
        |> Decode.map
            (\value ->
                case value of
                    0 ->
                        TypeUnknown

                    1 ->
                        TypeDouble

                    2 ->
                        TypeFloat

                    3 ->
                        TypeInt64

                    4 ->
                        TypeUint64

                    5 ->
                        TypeInt32

                    6 ->
                        TypeFixed64

                    7 ->
                        TypeFixed32

                    8 ->
                        TypeBool

                    9 ->
                        TypeString

                    10 ->
                        TypeGroup

                    11 ->
                        TypeMessage

                    12 ->
                        TypeBytes

                    13 ->
                        TypeUint32

                    14 ->
                        TypeEnum

                    15 ->
                        TypeSfixed32

                    16 ->
                        TypeSfixed64

                    17 ->
                        TypeSint32

                    18 ->
                        TypeSint64

                    v ->
                        KindUnrecognized_ v
            )


{-| -}
cardinalityDecoder : Decode.Decoder Cardinality
cardinalityDecoder =
    Decode.int32
        |> Decode.map
            (\value ->
                case value of
                    0 ->
                        CardinalityUnknown

                    1 ->
                        CardinalityOptional

                    2 ->
                        CardinalityRequired

                    3 ->
                        CardinalityRepeated

                    v ->
                        CardinalityUnrecognized_ v
            )


nullValueDecoder : Decode.Decoder NullValue
nullValueDecoder =
    Decode.int32
        |> Decode.map
            (\value ->
                case value of
                    0 ->
                        NullValue

                    v ->
                        NullValueUnrecognized_ v
            )


{-| -}
anyDecoder : Decode.Decoder Any
anyDecoder =
    Decode.message (Any "" (Encode.encode <| Encode.string ""))
        [ Decode.optional 1 Decode.string setTypeUrl
        , Decode.optional 2 Decode.bytes setValue
        ]


{-| -}
sourceContextDecoder : Decode.Decoder SourceContext
sourceContextDecoder =
    Decode.message (SourceContext "")
        [ Decode.optional 1 Decode.string setFileName
        ]


{-| -}
typeDecoder : Decode.Decoder Type
typeDecoder =
    Decode.message (Type "" [] [] [] Nothing SyntaxProto2)
        [ Decode.optional 1 Decode.string setName
        , Decode.repeated 2 fieldDecoder .fields setFields
        , Decode.repeated 3 Decode.string .oneofs setOneofs
        , Decode.repeated 4 optionDecoder .options setOptions
        , Decode.optional 5 (Decode.map Just sourceContextDecoder) setSourceContext
        , Decode.optional 6 syntaxDecoder setSyntax
        ]


{-| -}
fieldDecoder : Decode.Decoder Field
fieldDecoder =
    Decode.message (Field TypeUnknown CardinalityUnknown 0 "" "" 0 False [] "" "")
        [ Decode.optional 1 kindDecoder setKind
        , Decode.optional 2 cardinalityDecoder setCardinality
        , Decode.optional 3 Decode.int32 setNumber
        , Decode.optional 4 Decode.string setName
        , Decode.optional 6 Decode.string setTypeUrl
        , Decode.optional 7 Decode.int32 setOneofIndex
        , Decode.optional 8 Decode.bool setPacked
        , Decode.repeated 9 optionDecoder .options setOptions
        , Decode.optional 10 Decode.string setJsonName
        , Decode.optional 11 Decode.string setDefaultValue
        ]


{-| -}
enumDecoder : Decode.Decoder Enum
enumDecoder =
    Decode.message (Enum "" [] [] Nothing SyntaxProto2)
        [ Decode.optional 1 Decode.string setName
        , Decode.repeated 2 enumValueDecoder .enumvalue setEnumvalue
        , Decode.repeated 3 optionDecoder .options setOptions
        , Decode.optional 4 (Decode.map Just sourceContextDecoder) setSourceContext
        , Decode.optional 5 syntaxDecoder setSyntax
        ]


{-| -}
enumValueDecoder : Decode.Decoder EnumValue
enumValueDecoder =
    Decode.message (EnumValue "" 0 [])
        [ Decode.optional 1 Decode.string setName
        , Decode.optional 2 Decode.int32 setNumber
        , Decode.repeated 3 optionDecoder .options setOptions
        ]


{-| -}
optionDecoder : Decode.Decoder Option
optionDecoder =
    Decode.message (Option "" Nothing)
        [ Decode.optional 1 Decode.string setName
        , Decode.optional 2 (Decode.map Just anyDecoder) setValue
        ]


{-| -}
apiDecoder : Decode.Decoder Api
apiDecoder =
    Decode.message (Api "" [] [] "" Nothing [] SyntaxProto2)
        [ Decode.optional 1 Decode.string setName
        , Decode.repeated 2 methodDecoder .methods setMethods
        , Decode.repeated 3 optionDecoder .options setOptions
        , Decode.optional 4 Decode.string setVersion
        , Decode.optional 5 (Decode.map Just sourceContextDecoder) setSourceContext
        , Decode.repeated 6 mixinDecoder .mixins setMixins
        , Decode.optional 7 syntaxDecoder setSyntax
        ]


{-| -}
methodDecoder : Decode.Decoder Method
methodDecoder =
    Decode.message (Method "" "" False "" False [] SyntaxProto2)
        [ Decode.optional 1 Decode.string setName
        , Decode.optional 2 Decode.string setRequestTypeUrl
        , Decode.optional 3 Decode.bool setRequestStreaming
        , Decode.optional 4 Decode.string setResponseTypeUrl
        , Decode.optional 5 Decode.bool setResponseStreaming
        , Decode.repeated 6 optionDecoder .options setOptions
        , Decode.optional 7 syntaxDecoder setSyntax
        ]


{-| -}
mixinDecoder : Decode.Decoder Mixin
mixinDecoder =
    Decode.message (Mixin "" "")
        [ Decode.optional 1 Decode.string setName
        , Decode.optional 2 Decode.string setRoot
        ]


{-| -}
durationDecoder : Decode.Decoder Duration
durationDecoder =
    Decode.message (Duration 0 0)
        [ Decode.optional 1 Decode.int32 setSeconds
        , Decode.optional 2 Decode.int32 setNanos
        ]


{-| -}
emptyDecoder : Decode.Decoder Empty
emptyDecoder =
    Decode.message Empty
        []


{-| -}
fieldMaskDecoder : Decode.Decoder FieldMask
fieldMaskDecoder =
    Decode.message (FieldMask [])
        [ Decode.repeated 1 Decode.string .paths setPaths
        ]


unwrapStructFields : StructFields -> Dict.Dict String (Maybe Value)
unwrapStructFields (StructFields value) =
    value


{-| -}
structDecoder : Decode.Decoder Struct
structDecoder =
    Decode.message (Struct (StructFields Dict.empty))
        [ Decode.mapped 1 ( "", Nothing ) Decode.string (Decode.lazy (\_ -> Decode.map Just valueDecoder)) (unwrapStructFields << .fields) (setFields << StructFields)
        ]


unwrapValueKind : ValueKind -> Maybe KindType
unwrapValueKind (ValueKind value) =
    value


{-| -}
valueDecoder : Decode.Decoder Value
valueDecoder =
    Decode.message (Value (ValueKind Nothing))
        [ Decode.oneOf
            [ ( 1, Decode.lazy (\_ -> Decode.map KindNullValue nullValueDecoder) )
            , ( 2, Decode.lazy (\_ -> Decode.map KindNumberValue Decode.double) )
            , ( 3, Decode.lazy (\_ -> Decode.map KindStringValue Decode.string) )
            , ( 4, Decode.lazy (\_ -> Decode.map KindBoolValue Decode.bool) )
            , ( 5, Decode.lazy (\_ -> Decode.map KindStructValue structDecoder) )
            , ( 6, Decode.lazy (\_ -> Decode.map KindListValue listValueDecoder) )
            ]
            (setKind << ValueKind)
        ]


unwrapListValueValues : ListValueValues -> List Value
unwrapListValueValues (ListValueValues value) =
    value


{-| -}
listValueDecoder : Decode.Decoder ListValue
listValueDecoder =
    Decode.message (ListValue (ListValueValues []))
        [ Decode.repeated 1 (Decode.lazy (\_ -> valueDecoder)) (unwrapListValueValues << .values) (setValues << ListValueValues)
        ]


{-| -}
timestampDecoder : Decode.Decoder Timestamp
timestampDecoder =
    Decode.message (Timestamp 0 0)
        [ Decode.optional 1 Decode.int32 setSeconds
        , Decode.optional 2 Decode.int32 setNanos
        ]


{-| -}
doubleValueDecoder : Decode.Decoder DoubleValue
doubleValueDecoder =
    Decode.message (DoubleValue 0)
        [ Decode.optional 1 Decode.double setValue
        ]


{-| -}
floatValueDecoder : Decode.Decoder FloatValue
floatValueDecoder =
    Decode.message (FloatValue 0)
        [ Decode.optional 1 Decode.float setValue
        ]


{-| -}
int32ValueDecoder : Decode.Decoder Int32Value
int32ValueDecoder =
    Decode.message (Int32Value 0)
        [ Decode.optional 1 Decode.int32 setValue
        ]


{-| -}
uInt32ValueDecoder : Decode.Decoder UInt32Value
uInt32ValueDecoder =
    Decode.message (UInt32Value 0)
        [ Decode.optional 1 Decode.uint32 setValue
        ]


{-| -}
boolValueDecoder : Decode.Decoder BoolValue
boolValueDecoder =
    Decode.message (BoolValue False)
        [ Decode.optional 1 Decode.bool setValue
        ]


{-| -}
stringValueDecoder : Decode.Decoder StringValue
stringValueDecoder =
    Decode.message (StringValue "")
        [ Decode.optional 1 Decode.string setValue
        ]


{-| -}
bytesValueDecoder : Decode.Decoder BytesValue
bytesValueDecoder =
    Decode.message (BytesValue (Encode.encode <| Encode.string ""))
        [ Decode.optional 1 Decode.bytes setValue
        ]



-- ENCODER


{-| -}
toSyntaxEncoder : Syntax -> Encode.Encoder
toSyntaxEncoder value =
    Encode.int32 <|
        case value of
            SyntaxProto2 ->
                0

            SyntaxProto3 ->
                1

            SyntaxUnrecognized_ v ->
                v


{-| -}
toKindEncoder : Kind -> Encode.Encoder
toKindEncoder value =
    Encode.int32 <|
        case value of
            TypeUnknown ->
                0

            TypeDouble ->
                1

            TypeFloat ->
                2

            TypeInt64 ->
                3

            TypeUint64 ->
                4

            TypeInt32 ->
                5

            TypeFixed64 ->
                6

            TypeFixed32 ->
                7

            TypeBool ->
                8

            TypeString ->
                9

            TypeGroup ->
                10

            TypeMessage ->
                11

            TypeBytes ->
                12

            TypeUint32 ->
                13

            TypeEnum ->
                14

            TypeSfixed32 ->
                15

            TypeSfixed64 ->
                16

            TypeSint32 ->
                17

            TypeSint64 ->
                18

            KindUnrecognized_ v ->
                v


{-| -}
toCardinalityEncoder : Cardinality -> Encode.Encoder
toCardinalityEncoder value =
    Encode.int32 <|
        case value of
            CardinalityUnknown ->
                0

            CardinalityOptional ->
                1

            CardinalityRequired ->
                2

            CardinalityRepeated ->
                3

            CardinalityUnrecognized_ v ->
                v


toNullValueEncoder : NullValue -> Encode.Encoder
toNullValueEncoder value =
    Encode.int32 <|
        case value of
            NullValue ->
                0

            NullValueUnrecognized_ v ->
                v


{-| -}
toAnyEncoder : Any -> Encode.Encoder
toAnyEncoder model =
    Encode.message
        [ ( 1, Encode.string model.typeUrl )
        , ( 2, Encode.bytes model.value )
        ]


{-| -}
toSourceContextEncoder : SourceContext -> Encode.Encoder
toSourceContextEncoder model =
    Encode.message
        [ ( 1, Encode.string model.fileName )
        ]


{-| -}
toTypeEncoder : Type -> Encode.Encoder
toTypeEncoder model =
    Encode.message
        [ ( 1, Encode.string model.name )
        , ( 2, Encode.list toFieldEncoder model.fields )
        , ( 3, Encode.list Encode.string model.oneofs )
        , ( 4, Encode.list toOptionEncoder model.options )
        , ( 5, (Maybe.withDefault Encode.none << Maybe.map toSourceContextEncoder) model.sourceContext )
        , ( 6, toSyntaxEncoder model.syntax )
        ]


{-| -}
toFieldEncoder : Field -> Encode.Encoder
toFieldEncoder model =
    Encode.message
        [ ( 1, toKindEncoder model.kind )
        , ( 2, toCardinalityEncoder model.cardinality )
        , ( 3, Encode.int32 model.number )
        , ( 4, Encode.string model.name )
        , ( 6, Encode.string model.typeUrl )
        , ( 7, Encode.int32 model.oneofIndex )
        , ( 8, Encode.bool model.packed )
        , ( 9, Encode.list toOptionEncoder model.options )
        , ( 10, Encode.string model.jsonName )
        , ( 11, Encode.string model.defaultValue )
        ]


{-| -}
toEnumEncoder : Enum -> Encode.Encoder
toEnumEncoder model =
    Encode.message
        [ ( 1, Encode.string model.name )
        , ( 2, Encode.list toEnumValueEncoder model.enumvalue )
        , ( 3, Encode.list toOptionEncoder model.options )
        , ( 4, (Maybe.withDefault Encode.none << Maybe.map toSourceContextEncoder) model.sourceContext )
        , ( 5, toSyntaxEncoder model.syntax )
        ]


{-| -}
toEnumValueEncoder : EnumValue -> Encode.Encoder
toEnumValueEncoder model =
    Encode.message
        [ ( 1, Encode.string model.name )
        , ( 2, Encode.int32 model.number )
        , ( 3, Encode.list toOptionEncoder model.options )
        ]


{-| -}
toOptionEncoder : Option -> Encode.Encoder
toOptionEncoder model =
    Encode.message
        [ ( 1, Encode.string model.name )
        , ( 2, (Maybe.withDefault Encode.none << Maybe.map toAnyEncoder) model.value )
        ]


{-| -}
toApiEncoder : Api -> Encode.Encoder
toApiEncoder model =
    Encode.message
        [ ( 1, Encode.string model.name )
        , ( 2, Encode.list toMethodEncoder model.methods )
        , ( 3, Encode.list toOptionEncoder model.options )
        , ( 4, Encode.string model.version )
        , ( 5, (Maybe.withDefault Encode.none << Maybe.map toSourceContextEncoder) model.sourceContext )
        , ( 6, Encode.list toMixinEncoder model.mixins )
        , ( 7, toSyntaxEncoder model.syntax )
        ]


{-| -}
toMethodEncoder : Method -> Encode.Encoder
toMethodEncoder model =
    Encode.message
        [ ( 1, Encode.string model.name )
        , ( 2, Encode.string model.requestTypeUrl )
        , ( 3, Encode.bool model.requestStreaming )
        , ( 4, Encode.string model.responseTypeUrl )
        , ( 5, Encode.bool model.responseStreaming )
        , ( 6, Encode.list toOptionEncoder model.options )
        , ( 7, toSyntaxEncoder model.syntax )
        ]


{-| -}
toMixinEncoder : Mixin -> Encode.Encoder
toMixinEncoder model =
    Encode.message
        [ ( 1, Encode.string model.name )
        , ( 2, Encode.string model.root )
        ]


{-| -}
toDurationEncoder : Duration -> Encode.Encoder
toDurationEncoder model =
    Encode.message
        [ ( 1, Encode.int32 model.seconds )
        , ( 2, Encode.int32 model.nanos )
        ]


{-| -}
toEmptyEncoder : Empty -> Encode.Encoder
toEmptyEncoder model =
    Encode.message
        []


{-| -}
toFieldMaskEncoder : FieldMask -> Encode.Encoder
toFieldMaskEncoder model =
    Encode.message
        [ ( 1, Encode.list Encode.string model.paths )
        ]


{-| -}
toStructEncoder : Struct -> Encode.Encoder
toStructEncoder model =
    Encode.message
        [ ( 1, Encode.dict Encode.string (Maybe.withDefault Encode.none << Maybe.map toValueEncoder) (unwrapStructFields model.fields) )
        ]


toKindTypeEncoder : KindType -> ( Int, Encode.Encoder )
toKindTypeEncoder model =
    case model of
        KindNullValue value ->
            ( 1, toNullValueEncoder value )

        KindNumberValue value ->
            ( 2, Encode.double value )

        KindStringValue value ->
            ( 3, Encode.string value )

        KindBoolValue value ->
            ( 4, Encode.bool value )

        KindStructValue value ->
            ( 5, toStructEncoder value )

        KindListValue value ->
            ( 6, toListValueEncoder value )


{-| -}
toValueEncoder : Value -> Encode.Encoder
toValueEncoder model =
    Encode.message
        [ Maybe.withDefault ( 0, Encode.none ) <| Maybe.map toKindTypeEncoder (unwrapValueKind model.kind)
        ]


{-| -}
toListValueEncoder : ListValue -> Encode.Encoder
toListValueEncoder model =
    Encode.message
        [ ( 1, Encode.list toValueEncoder (unwrapListValueValues model.values) )
        ]


{-| -}
toTimestampEncoder : Timestamp -> Encode.Encoder
toTimestampEncoder model =
    Encode.message
        [ ( 1, Encode.int32 model.seconds )
        , ( 2, Encode.int32 model.nanos )
        ]


{-| -}
toDoubleValueEncoder : DoubleValue -> Encode.Encoder
toDoubleValueEncoder model =
    Encode.message
        [ ( 1, Encode.double model.value )
        ]


{-| -}
toFloatValueEncoder : FloatValue -> Encode.Encoder
toFloatValueEncoder model =
    Encode.message
        [ ( 1, Encode.float model.value )
        ]


{-| -}
toInt32ValueEncoder : Int32Value -> Encode.Encoder
toInt32ValueEncoder model =
    Encode.message
        [ ( 1, Encode.int32 model.value )
        ]


{-| -}
toUInt32ValueEncoder : UInt32Value -> Encode.Encoder
toUInt32ValueEncoder model =
    Encode.message
        [ ( 1, Encode.uint32 model.value )
        ]


{-| -}
toBoolValueEncoder : BoolValue -> Encode.Encoder
toBoolValueEncoder model =
    Encode.message
        [ ( 1, Encode.bool model.value )
        ]


{-| -}
toStringValueEncoder : StringValue -> Encode.Encoder
toStringValueEncoder model =
    Encode.message
        [ ( 1, Encode.string model.value )
        ]


{-| -}
toBytesValueEncoder : BytesValue -> Encode.Encoder
toBytesValueEncoder model =
    Encode.message
        [ ( 1, Encode.bytes model.value )
        ]



-- SETTERS


setTypeUrl : a -> { b | typeUrl : a } -> { b | typeUrl : a }
setTypeUrl value model =
    { model | typeUrl = value }


setValue : a -> { b | value : a } -> { b | value : a }
setValue value model =
    { model | value = value }


setFileName : a -> { b | fileName : a } -> { b | fileName : a }
setFileName value model =
    { model | fileName = value }


setName : a -> { b | name : a } -> { b | name : a }
setName value model =
    { model | name = value }


setFields : a -> { b | fields : a } -> { b | fields : a }
setFields value model =
    { model | fields = value }


setOneofs : a -> { b | oneofs : a } -> { b | oneofs : a }
setOneofs value model =
    { model | oneofs = value }


setOptions : a -> { b | options : a } -> { b | options : a }
setOptions value model =
    { model | options = value }


setSourceContext : a -> { b | sourceContext : a } -> { b | sourceContext : a }
setSourceContext value model =
    { model | sourceContext = value }


setSyntax : a -> { b | syntax : a } -> { b | syntax : a }
setSyntax value model =
    { model | syntax = value }


setKind : a -> { b | kind : a } -> { b | kind : a }
setKind value model =
    { model | kind = value }


setCardinality : a -> { b | cardinality : a } -> { b | cardinality : a }
setCardinality value model =
    { model | cardinality = value }


setNumber : a -> { b | number : a } -> { b | number : a }
setNumber value model =
    { model | number = value }


setOneofIndex : a -> { b | oneofIndex : a } -> { b | oneofIndex : a }
setOneofIndex value model =
    { model | oneofIndex = value }


setPacked : a -> { b | packed : a } -> { b | packed : a }
setPacked value model =
    { model | packed = value }


setJsonName : a -> { b | jsonName : a } -> { b | jsonName : a }
setJsonName value model =
    { model | jsonName = value }


setDefaultValue : a -> { b | defaultValue : a } -> { b | defaultValue : a }
setDefaultValue value model =
    { model | defaultValue = value }


setEnumvalue : a -> { b | enumvalue : a } -> { b | enumvalue : a }
setEnumvalue value model =
    { model | enumvalue = value }


setMethods : a -> { b | methods : a } -> { b | methods : a }
setMethods value model =
    { model | methods = value }


setVersion : a -> { b | version : a } -> { b | version : a }
setVersion value model =
    { model | version = value }


setMixins : a -> { b | mixins : a } -> { b | mixins : a }
setMixins value model =
    { model | mixins = value }


setRequestTypeUrl : a -> { b | requestTypeUrl : a } -> { b | requestTypeUrl : a }
setRequestTypeUrl value model =
    { model | requestTypeUrl = value }


setRequestStreaming : a -> { b | requestStreaming : a } -> { b | requestStreaming : a }
setRequestStreaming value model =
    { model | requestStreaming = value }


setResponseTypeUrl : a -> { b | responseTypeUrl : a } -> { b | responseTypeUrl : a }
setResponseTypeUrl value model =
    { model | responseTypeUrl = value }


setResponseStreaming : a -> { b | responseStreaming : a } -> { b | responseStreaming : a }
setResponseStreaming value model =
    { model | responseStreaming = value }


setRoot : a -> { b | root : a } -> { b | root : a }
setRoot value model =
    { model | root = value }


setSeconds : a -> { b | seconds : a } -> { b | seconds : a }
setSeconds value model =
    { model | seconds = value }


setNanos : a -> { b | nanos : a } -> { b | nanos : a }
setNanos value model =
    { model | nanos = value }


setPaths : a -> { b | paths : a } -> { b | paths : a }
setPaths value model =
    { model | paths = value }


setValues : a -> { b | values : a } -> { b | values : a }
setValues value model =
    { model | values = value }
