module ProtoBuf.Decode exposing
    ( Decoder, decode, message
    , required, optional, repeated, mapped, oneOf
    , int32, uint32, sint32, fixed32, sfixed32
    , double, float
    , string
    , bool
    , bytes
    , map
    )

{-| Library for turning [ProtoBuf](https://developers.google.com/protocol-buffers) messages into Elm values.


# Decoding

@docs Decoder, decode, message


# Field Decoders

@docs required, optional, repeated, mapped, oneOf


# Integers

@docs int32, uint32, sint32, fixed32, sfixed32


# Floats

@docs double, float


# Strings

@docs string


# Booleans

@docs bool


# Bytes

@docs bytes


# Map

@docs map

-}

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict exposing (Dict)
import Internal.ProtoBuf exposing (WireType(..))
import Internal.ProtoBuf.Decode exposing (..)



-- DECODER


{-| Describes how to turn a sequence of ProtoBuf-encoded bytes into a nice Elm value.
-}
type Decoder a
    = MessageDecoder (Int -> Decode.Decoder ( Message, a ))
    | Decoder WireType (Int -> Decode.Decoder ( Int, a ))



-- DECODE


{-| Turn a sequence of bytes into a nice Elm value.

     decode int32 <7F>    -- Just 127
     decode sint32 <7F>   -- Just -64
     decode sfixed32 <7F> -- Nothing

The `Decoder` specifies exactly how this should happen.
This process may fail if:

  - a required field is not present;
  - there is a mismatch of the [_wire type_](https://developers.google.com/protocol-buffers/docs/encoding#structure) of the encoded value and the decoder;
  - the sequence of bytes is corrupted or unexpected somehow.

The examples above show a case where there are not enough bytes.
They also show the same bytes sequence can lead to different values depending on the `Decoder` that is being used.
Decoders cannot always detect these kind of mismatches.

Values are always encoded together with a field number and the [_wire type_](https://developers.google.com/protocol-buffers/docs/encoding#structure).
This allows the decoder to connect the right fields and to read the right number of bytes.

    import ProtoBuf.Decode as Decode

    type alias Person =
        { age : Int
        , name : String
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        Decode.message Person
            |> Decode.optional 1 Decode.int32 0
            |> Decode.optional 2 Decode.string ""

You probably received these `Bytes` from an HTTP request.
As [`message`](#message) consumes **all remaing bytes** on the wire, you cannot use `Http.expectBytes` directly so you might want to use the `expectBytes` as provided here.

    import Http
    import ProtoBuf.Decode as Decode

    expectBytes : (Result Http.Error a -> msg) -> Decode.Decoder a -> Http.Expect msg
    expectBytes toMsg decoder =
        expectBytesResponse toMsg
            (\response ->
                case response of
                    Http.BadUrl_ url ->
                        Err (Http.BadUrl url)

                    Http.Timeout_ ->
                        Err Http.Timeout

                    Http.NetworkError_ ->
                        Err Http.NetworkError

                    Http.BadStatus_ metadata body ->
                        Err (Http.BadStatus metadata.statusCode)

                    Http.GoodStatus_ _ body ->
                        case Decode.decode decoder body of
                            Just value ->
                                Ok value

                            Nothing ->
                                Err (Http.BadBody "Decoder error")
            )

    getPerson : (Result Http.Error a -> msg) -> Cmd msg
    getPerson toMsg =
        Http.get
          { url = "https://example.com/person"
          , expectBytes toMsg personDecoder
          }

-}
decode : Decoder a -> Bytes -> Maybe a
decode decoder bs =
    case decoder of
        MessageDecoder decoder_ ->
            Decode.decode (Decode.map Tuple.second <| decoder_ (Bytes.width bs)) bs

        Decoder _ decoder_ ->
            Decode.decode (Decode.map Tuple.second <| decoder_ (Bytes.width bs)) bs


decodeAll : Decoder a -> List Bytes -> Maybe (List a)
decodeAll decoder chunks =
    case decoder of
        Decoder _ decoder_ ->
            decodeList decoder_ chunks

        decoder_ ->
            let
                values =
                    List.map (decode decoder_) chunks
                        |> List.filterMap identity
            in
            if List.length values == List.length chunks then
                Just values

            else
                Nothing


{-| Decode **all remaining bytes** into an record.
Chunks of bytes that are not needed to create the record are thrown away.
-}
message : a -> Decoder a
message v =
    Decoder LengthDelimited (\width -> Decode.succeed ( width, v ))



-- FIELD DECODERS


{-| Decode a required field.
This decoder will fail if the field is not present in the bytes sequence.

    import ProtoBuf.Decode as Decode

    type alias Person =
        { age : Int -- field number 1
        , name : String -- field number 3
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        Decode.message Person
            |> Decode.required 1 int32
            |> Decode.required 3 string

-}
required : Int -> Decoder a -> Decoder (a -> b) -> Decoder b
required fieldNumber fieldDecoder decoder =
    field (decodeRequired fieldNumber fieldDecoder) decoder


decodeRequired : Int -> Decoder a -> Message -> Maybe a
decodeRequired fieldNumber decoder data =
    case lastChunk fieldNumber (wireType decoder) data of
        FieldData bs ->
            decode decoder bs

        NotPresent ->
            Nothing

        Invalid ->
            Nothing


{-| Decode an optional field.
The decoder will fall back to the provided default value when the field is not present.
For `proto3` the default values can no longer be customized.

    import ProtoBuf.Decode as Decode

    type alias Person =
        { age : Int -- field number 2
        , name : String -- field number 4
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        Decode.message Person
            |> Decode.optional 2 int32 0
            |> Decode.optional 4 string ""

-}
optional : Int -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional fieldNumber fieldDecoder default decoder =
    field (decodeOptional fieldNumber fieldDecoder default) decoder


decodeOptional : Int -> Decoder a -> a -> Message -> Maybe a
decodeOptional fieldNumber decoder default data =
    case lastChunk fieldNumber (wireType decoder) data of
        FieldData bs ->
            decode decoder bs

        NotPresent ->
            Just default

        Invalid ->
            Nothing


{-| Decode a repeated field.
If no such fields are present, the decoder returns an empty list.

    import ProtoBuf.Decode as Decode

    type alias Person =
        { names : List String -- field number 5
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        Decode.message Person
            |> Decode.repeated 5 string

-}
repeated : Int -> Decoder a -> Decoder (List a -> b) -> Decoder b
repeated fieldNumber fieldDecoder decoder =
    field (decodeRepeated fieldNumber fieldDecoder) decoder


decodeRepeated : Int -> Decoder a -> Message -> Maybe (List a)
decodeRepeated fieldNumber decoder data =
    case allChunks fieldNumber LengthDelimited data of
        FieldData chunks ->
            decodeAll decoder chunks

        NotPresent ->
            Just []

        Invalid ->
            Nothing


{-| Decode a map.
If no such fields are present, the decoder returns an empty `Dict`.
Note that you need the provide two decoders here.
One for the keys and another for the values.
Keys without a value of values without a key fall back to the provided defaults.

    import ProtoBuf.Decode as Decode

    type alias Administration =
        { persons : Dict Int String -- field number 6
        }

    administrationDecoder : Decode.Decoder Administration
    administrationDecoder =
        Decode.message Administration
            |> Decode.mapped 6 int32 0 string ""

-}
mapped : Int -> Decoder comparable -> comparable -> Decoder a -> a -> Decoder (Dict comparable a -> b) -> Decoder b
mapped fieldNumber keyDecoder defaultKey valueDecoder defaultValue decoder =
    field (decodeMapped fieldNumber keyDecoder defaultKey valueDecoder defaultValue) decoder


decodeMapped : Int -> Decoder comparable -> comparable -> Decoder v -> v -> Message -> Maybe (Dict comparable v)
decodeMapped fieldNumber keyDecoder defaultKey valueDecoder defaultValue data =
    let
        decoder =
            message Tuple.pair
                |> optional 1 keyDecoder defaultKey
                |> optional 2 valueDecoder defaultValue
    in
    case allChunks fieldNumber (wireType decoder) data of
        FieldData chunks ->
            decodeAll decoder chunks
                |> Maybe.map Dict.fromList

        NotPresent ->
            Just Dict.empty

        Invalid ->
            Nothing


{-| Decode one of some fields.
The decoder will fall back to the provided default value when none of the fields is present.
As the decoder is capable of deserializing different types of data its return type must be a custom type.

    import ProtoBuf.Decode as Decode

    type Value
        = StringValue String
        | IntValue Int
        | NoValue

    valueDecoders : List ( Int, Decode.Decoder Value )
    valueDecoders =
        [ ( 8, Decode.string )
        , ( 9, Decode.int32 )
        ]

    type alias KeyValue =
        { key : String -- field number 7
        , value : Value -- field number 8 or 9
        }

    keyValueDecoder : Decode.Decoder KeyValue
    keyValueDecoder =
        Decode.message KeyValue
            |> Decode.optional 7 string ""
            |> Decode.oneOf valueDecoders NoValue

-}
oneOf : List ( Int, Decoder a ) -> a -> Decoder (a -> b) -> Decoder b
oneOf fieldDecoders default decoder =
    field (decodeOneOf fieldDecoders default) decoder


decodeOneOf : List ( Int, Decoder a ) -> a -> Message -> Maybe a
decodeOneOf decoders default data =
    case lastField (List.map (Tuple.mapSecond wireType) decoders) data of
        FieldData ( fieldNumber, bs ) ->
            decoders
                |> List.filter ((==) fieldNumber << Tuple.first)
                |> List.map Tuple.second
                |> List.head
                |> Maybe.andThen (\decoder -> decode decoder bs)

        NotPresent ->
            Just default

        Invalid ->
            Nothing


field : (Message -> Maybe a) -> Decoder (a -> b) -> Decoder b
field decodeField decoder =
    case decoder of
        MessageDecoder decoder_ ->
            MessageDecoder
                (\width ->
                    Decode.andThen
                        (\( data, toB ) ->
                            case decodeField data of
                                Just a ->
                                    Decode.succeed ( data, toB a )

                                Nothing ->
                                    Decode.fail
                        )
                        (decoder_ width)
                )

        Decoder _ decoder_ ->
            MessageDecoder (\width -> Decode.map2 (\dict ( _, v ) -> ( dict, v )) (messageDecoder width) (decoder_ width))
                |> field decodeField



-- INTEGER


{-| Decode a variable number of bytes into an integer from -2147483648 to 2147483647.
-}
int32 : Decoder Int
int32 =
    Decoder VarInt (always int32Decoder)


{-| Decode a variable number of bytes into an integer from 0 to 4294967295.
-}
uint32 : Decoder Int
uint32 =
    Decoder VarInt (always (Decode.map (Tuple.mapSecond unsigned) int32Decoder))


{-| Decode a variable number of bytes into an integer from -2147483648 to 2147483647.
-}
sint32 : Decoder Int
sint32 =
    Decoder VarInt (always (Decode.map (Tuple.mapSecond zigZag) int32Decoder))


{-| Decode four bytes into an integer from 0 to 4294967295.
-}
fixed32 : Decoder Int
fixed32 =
    Decoder VarInt (always (Decode.map (Tuple.pair 4) (Decode.unsignedInt32 LE)))


{-| Decode four bytes into an integer from -2147483648 to 2147483647.
-}
sfixed32 : Decoder Int
sfixed32 =
    Decoder VarInt (always (Decode.map (Tuple.pair 4) (Decode.signedInt32 LE)))



-- FLOAT


{-| Decode eight bytes into a floating point number.
-}
double : Decoder Float
double =
    Decoder Bit64 (always (Decode.map (Tuple.pair 8) (Decode.float64 LE)))


{-| Decode four bytes into a floating point number.
-}
float : Decoder Float
float =
    Decoder Bit32 (always (Decode.map (Tuple.pair 4) (Decode.float32 LE)))



-- STRING


{-| Decode all bytes into a string.
-}
string : Decoder String
string =
    Decoder LengthDelimited (\width -> Decode.map (Tuple.pair width) (Decode.string width))



-- BOOLEAN


{-| Decode one byte into a boolean.
-}
bool : Decoder Bool
bool =
    Decoder VarInt (always (Decode.map (Tuple.pair 1 << (/=) 0) Decode.unsignedInt8))



-- BYTES


{-| Copy all bytes into a new `Bytes` sequence.
-}
bytes : Decoder Bytes
bytes =
    Decoder LengthDelimited (\width -> Decode.map (Tuple.pair width) (Decode.bytes width))



-- MAP


{-| Transform the value produced by a decoder.
This is useful when encoding custom types as an enumeration:

    type Fruit
        = Apple
        | Banana
        | Mango
        | Unrecognized Int

    fruitDecoder : Decoder Fruit
    fruitDecoder =
        Decode.int32
            |> Decode.map
                (\value ->
                    case value of
                        0 ->
                            Apple

                        1 ->
                            Banana

                        2 ->
                            Mango

                        v ->
                            Unrecognized v
                )

`Unrecognized Int` is only used for values that are present but not known. For `proto2` decoding, it is left out and unknown values are decoded as the default type.
The first type in the `.proto` file is the default type (in this case, `Apple`).

-}
map : (a -> b) -> Decoder a -> Decoder b
map fn decoder =
    case decoder of
        MessageDecoder decoder_ ->
            MessageDecoder (\width -> Decode.map (Tuple.mapSecond fn) (decoder_ width))

        Decoder wireType_ decoder_ ->
            Decoder wireType_ (\width -> Decode.map (Tuple.mapSecond fn) (decoder_ width))



-- HELPERS


decodeList : (Int -> Decode.Decoder ( Int, a )) -> List Bytes -> Maybe (List a)
decodeList decoder chunks =
    case chunks of
        bs :: nextChunks ->
            Decode.decode (packedDecoder (Bytes.width bs) decoder) bs
                |> Maybe.andThen
                    (\values_ ->
                        decodeList decoder nextChunks
                            |> Maybe.map (\values -> values ++ values_)
                    )

        [] ->
            Just []


packedDecoder : Int -> (Int -> Decode.Decoder ( Int, a )) -> Decode.Decoder (List a)
packedDecoder width decoder =
    Decode.loop ( width, [] )
        (\( width_, values ) ->
            decoder width
                |> Decode.map (Tuple.mapBoth (\n -> width_ - n) (\v -> values ++ [ v ]))
                |> Decode.map
                    (\state ->
                        if Tuple.first state <= 0 then
                            Decode.Done (Tuple.second state)

                        else
                            Decode.Loop state
                    )
        )


unsigned : Int -> Int
unsigned value =
    if value < 0 then
        value + 2 ^ 32

    else
        value


zigZag : Int -> Int
zigZag value =
    Bitwise.xor (Bitwise.shiftRightZfBy 1 value) (-1 * Bitwise.and 1 value)


wireType : Decoder a -> WireType
wireType decoder =
    case decoder of
        MessageDecoder _ ->
            LengthDelimited

        Decoder wireType_ _ ->
            wireType_
