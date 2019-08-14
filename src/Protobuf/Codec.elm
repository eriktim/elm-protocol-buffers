module Protobuf.Codec exposing
    ( Codec, decode, encode
    , CodecBuilder, builder, field, required, optional, repeated, repeatedInefficiently, map, oneof, build
    , OneofCodec, OneofCodecBuilder, oneofBuilder, FieldEncoder, oneofField, buildOneof
    , int32, uint32, sint32, fixed32, sfixed32
    , double, float
    , string
    , bool
    , bytes
    , invmap, maybe, lazy
    )

{-| Library for turning
[Protobuf](https://developers.google.com/protocol-buffers) messages into Elm
values and vice versa.

> The examples show `Bytes` values like this: `<3A* 05* 68 65 6C 6C 6F>`. The
> `*` means the byte is Protobuf _metadata_. It does not contain any real
> value. Here `3A` means the next encoded field is a _length delimited_ value
> for field number `7`. `05` is the number of bytes that was used to encode the
> value that follows. Those five bytes contain the string `hello`. Read
> [this](https://developers.google.com/protocol-buffers/docs/encoding) if
> you want to learn more about how Protobuf encoding works.


# Codec

@docs Codec, decode, encode


# Codec Builder

@docs CodecBuilder, builder, field, required, optional, repeated, repeatedInefficiently, map, oneof, build


# Oneof Builder

@docs OneofCodec, OneofCodecBuilder, oneofBuilder, FieldEncoder, oneofField, buildOneof


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


# Special

@docs invmap, maybe, lazy

-}

-- DECODER

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Dict exposing (Dict)
import Http
import Internal.Decode as Decode
import Internal.Encode as Encode
import Internal.Protobuf as Protobuf exposing (..)
import Set


{-| -}
type Codec a
    = Codec
        { default : Default a
        , decoder : Decode.Decoder a
        , encoder : a -> Encode.Encoder
        }


type Default a
    = Default a (a -> Bool)
    | NoDefault


buildCodec : Default a -> Decode.Decoder a -> (a -> Encode.Encoder) -> Codec a
buildCodec defaultValue decoder encoder =
    Codec { default = defaultValue, decoder = decoder, encoder = encoder }


default : a -> Default a
default a =
    Default a ((==) a)


invmapDefault : (a -> b) -> (b -> a) -> Default a -> Default b
invmapDefault ab ba d =
    case d of
        Default v m ->
            Default (ab v) (m << ba)

        NoDefault ->
            NoDefault


{-| -}
decode : Codec a -> Bytes.Bytes -> Maybe a
decode (Codec { decoder }) bs =
    Decode.decode decoder bs


{-| -}
encode : Codec a -> a -> Bytes.Bytes
encode (Codec { encoder }) value =
    Encode.encode (encoder value)


{-| -}
type CodecBuilder f a
    = CodecBuilder
        { decoder : Decode.Decoder (Protobuf.Message f)
        , encoders : Protobuf.Message a -> List Encode.FieldEncoder
        }


{-| -}
type alias FieldEncoder =
    Encode.FieldEncoder


{-| -}
builder : f -> CodecBuilder f a
builder f =
    CodecBuilder
        { decoder = Decode.message f
        , encoders = always []
        }


{-| -}
build : CodecBuilder a a -> Codec (Protobuf.Message a)
build (CodecBuilder c) =
    buildCodec NoDefault
        c.decoder
        (\( fieldData, model ) ->
            Encode.message fieldData (c.encoders ( fieldData, model ))
        )


fieldCodec : (Decode.Decoder ( FieldData, v -> f ) -> Decode.Decoder ( FieldData, f )) -> (v -> FieldEncoder) -> (a -> Maybe v) -> CodecBuilder (v -> f) a -> CodecBuilder f a
fieldCodec fieldDecoder fieldEncoder getter (CodecBuilder cb) =
    CodecBuilder
        { decoder =
            fieldDecoder cb.decoder
        , encoders =
            \( fieldData, model ) ->
                case getter model of
                    Just v ->
                        fieldEncoder v :: cb.encoders ( fieldData, model )

                    Nothing ->
                        cb.encoders ( fieldData, model )
        }


{-| -}
field : Int -> Codec v -> (a -> v) -> CodecBuilder (v -> f) a -> CodecBuilder f a
field fieldNumber (Codec c) getter =
    let
        ( defaultValue, matches ) =
            case c.default of
                Default v m ->
                    ( Just v, m )

                NoDefault ->
                    ( Nothing, always False )

        maybeGetter model =
            if matches (getter model) then
                Nothing

            else
                Just (getter model)
    in
    fieldCodec
        (Decode.field fieldNumber c.decoder defaultValue)
        (Tuple.pair fieldNumber << c.encoder)
        maybeGetter


{-| -}
optional : Int -> Codec v -> (a -> v) -> Maybe v -> CodecBuilder (v -> f) a -> CodecBuilder f a
optional fieldNumber (Codec c) getter maybeDefault =
    fieldCodec
        (Decode.field fieldNumber c.decoder maybeDefault)
        (Tuple.pair fieldNumber << c.encoder)
        (Just << getter)


{-| -}
required : Int -> Codec v -> (a -> v) -> CodecBuilder (v -> f) a -> CodecBuilder f a
required fieldNumber (Codec c) getter =
    fieldCodec
        (Decode.field fieldNumber c.decoder Nothing)
        (Tuple.pair fieldNumber << c.encoder)
        (Just << getter)


{-| -}
repeated : Int -> Codec v -> (a -> List v) -> CodecBuilder (List v -> f) a -> CodecBuilder f a
repeated fieldNumber (Codec c) getter =
    fieldCodec
        (Decode.repeated fieldNumber c.decoder)
        (Tuple.pair fieldNumber << Encode.list Encode.AutoPacking c.encoder)
        (Just << getter)


{-| -}
repeatedInefficiently : Int -> Codec v -> (a -> List v) -> CodecBuilder (List v -> f) a -> CodecBuilder f a
repeatedInefficiently fieldNumber (Codec c) getter =
    fieldCodec
        (Decode.repeated fieldNumber c.decoder)
        (Tuple.pair fieldNumber << Encode.list Encode.NoPacking c.encoder)
        (Just << getter)


{-| -}
map : Int -> Codec comparable -> Codec v -> (a -> Dict.Dict comparable v) -> CodecBuilder (Dict.Dict comparable v -> f) a -> CodecBuilder f a
map fieldNumber (Codec ck) (Codec cv) getter =
    let
        defaultKey =
            case ck.default of
                Default v _ ->
                    Just v

                NoDefault ->
                    Nothing

        defaultValue =
            case cv.default of
                Default v _ ->
                    Just v

                NoDefault ->
                    Nothing
    in
    fieldCodec
        (Decode.dict fieldNumber ck.decoder cv.decoder ( defaultKey, defaultValue ))
        (Tuple.pair fieldNumber << Encode.dict ck.encoder cv.encoder)
        (Just << getter)


{-| -}
oneof : OneofCodec v -> (a -> Maybe v) -> CodecBuilder (Maybe v -> f) a -> CodecBuilder f a
oneof (OneofCodec c) getter =
    fieldCodec
        (Decode.oneOf c.decoders)
        (Maybe.withDefault ( 0, Encode.none ) << Maybe.map c.encoder)
        (Just << getter)


{-| -}
type OneofCodec a
    = OneofCodec
        { decoders : List ( Int, Decode.Decoder a )
        , encoder : a -> FieldEncoder
        }


{-| -}
type OneofCodecBuilder oneofEncoder a
    = OneofCodecBuilder
        { decoders : List ( Int, Decode.Decoder a )
        , encoder : oneofEncoder
        }


{-| -}
oneofBuilder : oneofEncoder -> OneofCodecBuilder oneofEncoder a
oneofBuilder encoder =
    OneofCodecBuilder
        { decoders = []
        , encoder = encoder
        }


{-| -}
buildOneof : OneofCodecBuilder (a -> FieldEncoder) a -> OneofCodec a
buildOneof (OneofCodecBuilder cb) =
    OneofCodec
        { decoders = cb.decoders
        , encoder = cb.encoder
        }


{-| -}
oneofField : Int -> (v -> a) -> Codec v -> OneofCodecBuilder ((v -> FieldEncoder) -> oneofEncoder) a -> OneofCodecBuilder oneofEncoder a
oneofField fieldNumber fn (Codec c) (OneofCodecBuilder cb) =
    OneofCodecBuilder
        { decoders = ( fieldNumber, Decode.map fn c.decoder ) :: cb.decoders
        , encoder = cb.encoder (Tuple.pair fieldNumber << c.encoder)
        }


{-| -}
lazy : (() -> Codec a) -> Codec a
lazy delayed =
    buildCodec NoDefault
        (Decode.lazy
            (\_ ->
                case delayed () of
                    Codec { decoder } ->
                        decoder
            )
        )
        (\message ->
            case delayed () of
                Codec { encoder } ->
                    encoder message
        )


{-| -}
maybe : Codec a -> Codec (Maybe a)
maybe (Codec c) =
    buildCodec (Default Nothing (always False))
        (Decode.map Just c.decoder)
        (\message ->
            case message of
                Just m ->
                    c.encoder m

                Nothing ->
                    Encode.none
        )


{-| -}
invmap : (a -> b) -> (b -> a) -> Codec a -> Codec b
invmap ab ba (Codec c) =
    buildCodec
        (invmapDefault ab ba c.default)
        (Decode.map ab c.decoder)
        (c.encoder << ba)


{-| Codec for integers from `-2147483648` to `2147483647`. Uses
variable-length encoding. Inefficient for encoding negative numbers – if your
field is likely to have negative values, use [`sint32`](#sint32) instead.

     0    -- <00>
     100  -- <64>
     -100 -- <FF FF FF FF FF FF FF 9C>

-}
int32 : Codec Int
int32 =
    buildCodec (default 0) Decode.int32 Encode.int32


{-| Codec for integers from `0` to `4294967295`.
Uses variable-length encoding.

     0   -- <00>
     100 -- <64>

-}
uint32 : Codec Int
uint32 =
    buildCodec (default 0) Decode.uint32 Encode.uint32


{-| Codec for integers from `-2147483648` to `2147483647`. Uses
variable-length encoding. These encoder encodes negative numbers more
efficiently than [`int32`](#int32).

     0    -- <00>
     100  -- <C8 01>
     -100 -- <C7 01>

-}
sint32 : Codec Int
sint32 =
    buildCodec (default 0) Decode.sint32 Encode.sint32


{-| Codec for integers from `0` to `4294967295`. Always four bytes.
More efficient than [`uint32`](#uint32) if values are often greater than
`268435456`.

     0   -- <00 00 00 00>
     100 -- <64 00 00 00>

-}
fixed32 : Codec Int
fixed32 =
    buildCodec (default 0) Decode.fixed32 Encode.fixed32


{-| Codec for integers from `-2147483648` to `2147483647`.
Always four bytes.

     0    -- <00 00 00 00>
     100  -- <64 00 00 00>
     -100 -- <9C FF FF FF>

-}
sfixed32 : Codec Int
sfixed32 =
    buildCodec (default 0) Decode.sfixed32 Encode.sfixed32


{-| Codec for 64-bit floating point numbers.

     0    -- <00 00 00 00 00 00 00 00>
     100  -- <00 00 00 00 00 00 59 40>
     -100 -- <00 00 00 00 00 00 59 C0>

-}
double : Codec Float
double =
    buildCodec (default 0) Decode.double Encode.double


{-| Codec for 32-bit floating point numbers.
The value may lose some precision by encoding it as a float.

    0    -- <00 00 00 00>
    100  -- <00 00 C8 42>
    -100 -- <00 00 C8 C2>

-}
float : Codec Float
float =
    buildCodec (default 0) Decode.float Encode.float


{-| Codec for strings.

     "$20"   -- <24 32 30>
     "£20"   -- <C2 A3 32 30>
     "€20"   -- <E2 82 AC 32 30>
     "bread" -- <62 72 65 61 64>
     "brød"  -- <62 72 C3 B8 64>

-}
string : Codec String
string =
    buildCodec (default "") Decode.string Encode.string


{-| Codec for booleans.

     False -- <00>
     True  -- <01>

-}
bool : Codec Bool
bool =
    buildCodec (default False) Decode.bool Encode.bool


{-| Codec for raw bytes.

     Hex.toBytes "010203" -- <01 02 03>

-}
bytes : Codec Bytes.Bytes
bytes =
    buildCodec
        (Default emptyBytes ((==) 0 << Bytes.width))
        Decode.bytes
        Encode.bytes
