module ProtoBuf.Decode exposing
    ( Decoder
    , FieldDecoder
    , bool
    , bytes
    , decode
    , double
    , embedded
    , field
    , fixed32
    , float
    , int32
    , message
    , repeated
    , sfixed32
    , sint32
    , string
    , uint32
    )

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict
import Internal.ProtoBuf exposing (WireType(..))


type alias Chunks =
    List Bytes


type alias ChunksDict =
    Dict.Dict Int Chunks


type Decoder a
    = Decoder (ChunksDict -> Maybe a)


type FieldDecoder a
    = FieldDecoder a (Chunks -> Maybe ( Chunks, a ))


decode : Decoder a -> Bytes -> Maybe a
decode (Decoder decoder) bs =
    Decode.decode (chunksDictDecoder (Bytes.width bs)) bs
        |> Maybe.andThen decoder


decodeField : FieldDecoder a -> Chunks -> Maybe a
decodeField decoder chunks =
    case decoder of
        FieldDecoder _ decoder_ ->
            Maybe.map Tuple.second (decoder_ chunks)


message : a -> Decoder a
message type_ =
    Decoder (\_ -> Just type_)


field : Int -> FieldDecoder a -> Decoder (a -> b) -> Decoder b
field fieldNumber fieldDecoder_ (Decoder decoder) =
    Decoder
        (\dict ->
            case Dict.get fieldNumber dict of
                Just chunks ->
                    Maybe.map2 (\v fn -> fn v) (decodeField fieldDecoder_ chunks) (decoder dict)

                Nothing ->
                    Maybe.map (\fn -> fn (defaultValue fieldDecoder_)) (decoder dict)
        )


embedded : Decoder a -> FieldDecoder (Maybe a)
embedded decoder =
    FieldDecoder Nothing
        (\chunks ->
            case chunks of
                bs :: others ->
                    decode decoder bs
                        |> Maybe.map (\v -> ( others, Just v ))

                [] ->
                    Nothing
        )


repeated : FieldDecoder a -> FieldDecoder (List a)
repeated decoder =
    case decoder of
        FieldDecoder _ decoder_ ->
            FieldDecoder [] (decodeRepeatedField decoder_)


decodeRepeatedField : (Chunks -> Maybe ( Chunks, a )) -> Chunks -> Maybe ( Chunks, List a )
decodeRepeatedField decoder chunks =
    case chunks of
        bs :: chunks_ ->
            Maybe.map2 (\v ( _, w ) -> ( [], w ++ v )) (decodeRepeatedField_ decoder [] [ bs ]) (decodeRepeatedField decoder chunks_)

        [] ->
            Just ( [], [] )


decodeRepeatedField_ : (Chunks -> Maybe ( Chunks, a )) -> List a -> Chunks -> Maybe (List a)
decodeRepeatedField_ decoder values chunks =
    Maybe.andThen
        (\( chunks_, value ) ->
            if List.isEmpty chunks_ then
                Just (values ++ [ value ])

            else
                decodeRepeatedField_ decoder (values ++ [ value ]) chunks_
        )
        (decoder chunks)


fieldDecoder : a -> (Int -> Decode.Decoder ( Int, a )) -> FieldDecoder a
fieldDecoder default decoder =
    FieldDecoder default
        (\chunks ->
            case chunks of
                bs :: other ->
                    let
                        width =
                            Bytes.width bs
                    in
                    Decode.decode (decoder width) bs
                        |> Maybe.andThen
                            (\( n, value ) ->
                                let
                                    n_ =
                                        width - n
                                in
                                if n_ > 0 then
                                    Decode.decode
                                        (Decode.andThen
                                            (\_ -> Decode.map (\bs_ -> ( bs_ :: other, value )) (Decode.bytes n_))
                                            (Decode.bytes n)
                                        )
                                        bs

                                else
                                    Just ( other, value )
                            )

                [] ->
                    Just ( [], default )
        )


double : FieldDecoder Float
double =
    fieldDecoder 0 (\_ -> Decode.map (Tuple.pair 8) (Decode.float64 LE))


float : FieldDecoder Float
float =
    fieldDecoder 0 (\_ -> Decode.map (Tuple.pair 4) (Decode.float32 LE))


int32 : FieldDecoder Int
int32 =
    fieldDecoder 0 (\_ -> int32Decoder)


uint32 : FieldDecoder Int
uint32 =
    fieldDecoder 0 (\_ -> Decode.map (Tuple.mapSecond unsignedDecode) int32Decoder)


sint32 : FieldDecoder Int
sint32 =
    fieldDecoder 0 (\_ -> Decode.map (Tuple.mapSecond zigZagDecode) int32Decoder)


fixed32 : FieldDecoder Int
fixed32 =
    fieldDecoder 0 (\_ -> Decode.map (Tuple.pair 4) (Decode.unsignedInt32 LE))


sfixed32 : FieldDecoder Int
sfixed32 =
    fieldDecoder 0 (\_ -> Decode.map (Tuple.pair 4) (Decode.signedInt32 LE))


bool : FieldDecoder Bool
bool =
    fieldDecoder False (\_ -> Decode.map (Tuple.pair 1 << (/=) 0) Decode.unsignedInt8)


string : FieldDecoder String
string =
    fieldDecoder "" (\width -> Decode.map (Tuple.pair width) (Decode.string width))


bytes : FieldDecoder Bytes
bytes =
    fieldDecoder emptyBytes (\width -> Decode.map (Tuple.pair width) (Decode.bytes width))


emptyBytes : Bytes
emptyBytes =
    Encode.encode (Encode.sequence [])



-- HELPERS


tagDecoder : Decode.Decoder ( Int, Int, WireType )
tagDecoder =
    int32Decoder
        |> Decode.andThen
            (\( usedBytes, value ) ->
                let
                    wireType =
                        case Bitwise.and 0x07 value of
                            0 ->
                                Just VarInt

                            1 ->
                                Just Bit64

                            2 ->
                                Just LengthDelimited

                            3 ->
                                Just StartGroup

                            4 ->
                                Just EndGroup

                            5 ->
                                Just Bit32

                            _ ->
                                Nothing
                in
                case wireType of
                    Just wireType_ ->
                        Decode.succeed ( usedBytes, Bitwise.shiftRightZfBy 3 value, wireType_ )

                    Nothing ->
                        Decode.fail
            )


varIntListDecoder : Decode.Decoder (List Int)
varIntListDecoder =
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                if Bitwise.and 0x80 value == 0x80 then
                    Decode.map (\values -> value :: values) varIntListDecoder

                else
                    Decode.succeed [ value ]
            )


int32Decoder : Decode.Decoder ( Int, Int )
int32Decoder =
    varIntListDecoder
        |> Decode.map
            (\values ->
                let
                    usedBytes =
                        List.length values

                    values_ =
                        List.map (Bitwise.and 0x7F) values

                    offsets =
                        List.map ((*) 7) <|
                            List.range 0 (List.length values)
                in
                ( usedBytes, List.sum <| List.map2 Bitwise.shiftLeftBy offsets values_ )
            )


unsignedDecode : Int -> Int
unsignedDecode value =
    if value < 0 then
        value + 2 ^ 32

    else
        value


zigZagDecode : Int -> Int
zigZagDecode value =
    Bitwise.xor (Bitwise.shiftRightZfBy 1 value) (-1 * Bitwise.and 1 value)


chunksDictDecoder : Int -> Decode.Decoder ChunksDict
chunksDictDecoder width =
    Decode.loop ( width, Dict.empty ) chunksDictStepDecoder


chunksDictStepDecoder : ( Int, ChunksDict ) -> Decode.Decoder (Decode.Step ( Int, ChunksDict ) ChunksDict)
chunksDictStepDecoder ( bytesRemaining, dict ) =
    if bytesRemaining <= 0 then
        Decode.succeed (Decode.Done dict)

    else
        chunkDecoder
            |> Decode.map
                (\( fieldNumber, usedBytes, chunk ) ->
                    let
                        newDict =
                            Dict.update fieldNumber
                                (\chunks ->
                                    Just <| chunk :: Maybe.withDefault [] chunks
                                )
                                dict

                        newBytesRemaining =
                            bytesRemaining - Bytes.width chunk - usedBytes
                    in
                    Decode.Loop ( newBytesRemaining, newDict )
                )


chunkDecoder : Decode.Decoder ( Int, Int, Bytes )
chunkDecoder =
    tagDecoder
        |> Decode.andThen
            (\( usedBytes, fieldNumber, wireType ) ->
                case wireType of
                    VarInt ->
                        varIntListDecoder
                            |> Decode.map
                                (\values ->
                                    let
                                        value =
                                            Encode.encode <| Encode.sequence (List.map Encode.unsignedInt8 values)
                                    in
                                    ( fieldNumber, List.length values + usedBytes, value )
                                )

                    Bit64 ->
                        Decode.bytes 8
                            |> Decode.map (\value -> ( fieldNumber, usedBytes, value ))

                    LengthDelimited ->
                        int32Decoder
                            |> Decode.andThen
                                (\( n, length ) ->
                                    Decode.bytes length
                                        |> Decode.map
                                            (\bytes_ ->
                                                ( fieldNumber, n + usedBytes, bytes_ )
                                            )
                                )

                    StartGroup ->
                        Decode.fail

                    EndGroup ->
                        Decode.fail

                    Bit32 ->
                        Decode.bytes 4
                            |> Decode.map
                                (\bs ->
                                    ( fieldNumber, usedBytes, bs )
                                )
            )


defaultValue : FieldDecoder a -> a
defaultValue decoder =
    case decoder of
        FieldDecoder default _ ->
            default
