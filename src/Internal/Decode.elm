module Internal.Decode exposing
    ( ConsumedBytes(..)
    , Decoder
    , bool
    , bytes
    , custom
    , decode
    , dict
    , double
    , field
    , fixed32
    , float
    , int32
    , lazy
    , map
    , message
    , oneOf
    , repeated
    , sfixed32
    , sint32
    , string
    , uint32
    )

import Bitwise
import Bytes
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict
import Internal.Protobuf exposing (..)
import Set



-- DECODER


type Decoder a
    = Decoder (WireType -> Decode.Decoder ( ConsumedBytes, a ))


type ConsumedBytes
    = ConsumedBytes Int
    | ConsumedChunk
    | ConsumedNone



-- DECODE


decode : Decoder a -> Bytes.Bytes -> Maybe a
decode (Decoder decoder) bs =
    let
        wireType =
            LengthDelimited (Bytes.width bs)
    in
    Decode.decode (decoder wireType) bs
        |> Maybe.map Tuple.second


message : a -> Decoder ( FieldData, a )
message v =
    Decoder
        (\wireType ->
            case wireType of
                LengthDelimited width ->
                    fieldDataDecoder width
                        |> Decode.map (\fieldData -> ( ConsumedBytes width, ( fieldData, v ) ))

                _ ->
                    Decode.fail
        )



-- FIELD DECODERS


fieldDecoder : (Int -> Bool) -> (FieldData -> Maybe ( ConsumedBytes, v )) -> Decoder ( FieldData, v -> a ) -> Decoder ( FieldData, a )
fieldDecoder predicate decodeFields (Decoder d) =
    Decoder
        (\wireType ->
            d wireType
                |> Decode.andThen
                    (\( _, ( fieldData, f ) ) ->
                        let
                            ( fields, newFieldData ) =
                                List.partition (predicate << Tuple.first) fieldData
                        in
                        case decodeFields fields of
                            Just ( ConsumedNone, value ) ->
                                Decode.succeed ( ConsumedNone, ( fieldData, f value ) )

                            Just ( consumedBytes, value ) ->
                                Decode.succeed ( consumedBytes, ( newFieldData, f value ) )

                            Nothing ->
                                Decode.fail
                    )
        )


decodeChunk : Decoder a -> Chunk -> Maybe ( ConsumedBytes, a )
decodeChunk (Decoder decoder) ( wireType, bs ) =
    Decode.decode (decoder wireType) bs


field : Int -> Decoder v -> Maybe v -> Decoder ( FieldData, v -> a ) -> Decoder ( FieldData, a )
field fieldNumber decoder default =
    fieldDecoder
        ((==) fieldNumber)
        (\fields ->
            case Maybe.map Tuple.second (List.head fields) of
                Just chunk ->
                    decodeChunk decoder chunk

                Nothing ->
                    case default of
                        Just v ->
                            Just ( ConsumedChunk, v )

                        Nothing ->
                            Nothing
        )


repeated : Int -> Decoder v -> Decoder ( FieldData, List v -> a ) -> Decoder ( FieldData, a )
repeated fieldNumber (Decoder decoder) =
    let
        listDecoder =
            Decoder
                (\wireType ->
                    case wireType of
                        LengthDelimited width ->
                            Decode.loop ( width, [] ) (stepPackedField width (decoder wireType))

                        _ ->
                            Decode.map (Tuple.mapSecond List.singleton) (decoder wireType)
                )
    in
    fieldDecoder
        ((==) fieldNumber)
        (\fields ->
            List.map Tuple.second fields
                |> List.foldr
                    (\chunk values ->
                        Maybe.map2 (++) values (Maybe.map Tuple.second <| decodeChunk listDecoder chunk)
                    )
                    (Just [])
                |> Maybe.map (Tuple.pair ConsumedChunk)
        )


dict : Int -> Decoder comparable -> Decoder v -> ( Maybe comparable, Maybe v ) -> Decoder ( FieldData, Dict.Dict comparable v -> a ) -> Decoder ( FieldData, a )
dict fieldNumber keyDecoder valueDecoder ( defaultKey, defaultValue ) =
    let
        dictDecoder =
            message Tuple.pair
                |> field 1 keyDecoder defaultKey
                |> field 2 valueDecoder defaultValue
    in
    fieldDecoder
        ((==) fieldNumber)
        (\fields ->
            List.map Tuple.second fields
                |> List.foldr
                    (\chunk values ->
                        Maybe.map2 (++) values (Maybe.map (List.singleton << Tuple.second << Tuple.second) <| decodeChunk dictDecoder chunk)
                    )
                    (Just [])
                |> Maybe.map (Tuple.pair ConsumedChunk << Dict.fromList)
        )


oneOf : List ( Int, Decoder v ) -> Decoder ( FieldData, Maybe v -> a ) -> Decoder ( FieldData, a )
oneOf decoders =
    let
        fieldNumbers =
            List.map Tuple.first decoders
    in
    fieldDecoder
        (\n -> List.member n fieldNumbers)
        (\fields ->
            case fields of
                ( fieldNumber, chunk ) :: _ ->
                    case List.head (List.filter ((==) fieldNumber << Tuple.first) decoders) of
                        Just ( _, decoder ) ->
                            decodeChunk decoder chunk
                                |> Maybe.map (Tuple.mapSecond Just)

                        Nothing ->
                            Nothing

                [] ->
                    Just ( ConsumedNone, Nothing )
        )



-- PRIMITIVES


int32 : Decoder Int
int32 =
    packedDecoder VarInt varIntDecoder


uint32 : Decoder Int
uint32 =
    packedDecoder VarInt (Decode.map (Tuple.mapSecond unsigned32) varIntDecoder)


sint32 : Decoder Int
sint32 =
    packedDecoder VarInt (Decode.map (Tuple.mapSecond zigZag32) varIntDecoder)


fixed32 : Decoder Int
fixed32 =
    packedDecoder Bit32 (Decode.map (Tuple.pair 4) (Decode.unsignedInt32 Bytes.LE))


sfixed32 : Decoder Int
sfixed32 =
    packedDecoder Bit32 (Decode.map (Tuple.pair 4) (Decode.signedInt32 Bytes.LE))


double : Decoder Float
double =
    packedDecoder Bit64 (Decode.map (Tuple.pair 8) (Decode.float64 Bytes.LE))


float : Decoder Float
float =
    packedDecoder Bit32 (Decode.map (Tuple.pair 4) (Decode.float32 Bytes.LE))


string : Decoder String
string =
    lengthDelimitedDecoder Decode.string


bool : Decoder Bool
bool =
    packedDecoder VarInt (Decode.map (Tuple.mapSecond ((/=) 0)) varIntDecoder)


bytes : Decoder Bytes.Bytes
bytes =
    lengthDelimitedDecoder Decode.bytes



-- CUSTOM


custom : Decoder a -> (a -> Maybe ( ConsumedBytes, b )) -> Decoder b
custom (Decoder decoder) fn =
    Decoder
        (\wireType ->
            decoder wireType
                |> Decode.map (fn << Tuple.second)
                |> Decode.andThen (Maybe.withDefault Decode.fail << Maybe.map Decode.succeed)
        )



-- MAP


map : (a -> b) -> Decoder a -> Decoder b
map fn (Decoder decoder) =
    Decoder (Decode.map (Tuple.mapSecond fn) << decoder)



-- LAZY


lazy : (() -> Decoder a) -> Decoder a
lazy delayedDecoder =
    Decoder
        (\wireType ->
            Decode.andThen
                (\v ->
                    case delayedDecoder v of
                        Decoder d ->
                            d wireType
                )
                (Decode.succeed ())
        )



-- BYTES DECODER


fieldDataDecoder : Int -> Decode.Decoder FieldData
fieldDataDecoder width =
    Decode.loop ( width, [] ) fieldDataStepDecoder


fieldDataStepDecoder : ( Int, FieldData ) -> Decode.Decoder (Decode.Step ( Int, FieldData ) FieldData)
fieldDataStepDecoder ( bytesRemaining, fieldData ) =
    if bytesRemaining <= 0 then
        Decode.succeed (Decode.Done fieldData)

    else
        fieldChunkDecoder
            |> Decode.map
                (\{ fieldNumber, bytesConsumed, chunk } ->
                    Decode.Loop ( bytesRemaining - bytesConsumed, ( fieldNumber, chunk ) :: fieldData )
                )


fieldChunkDecoder : Decode.Decoder { fieldNumber : Int, bytesConsumed : Int, chunk : Chunk }
fieldChunkDecoder =
    tagDecoder
        |> Decode.andThen
            (\( bytesConsumed, ( fieldNumber, wireType ) ) ->
                chunkDecoder wireType
                    |> Decode.map
                        (\( n, chunk ) ->
                            { fieldNumber = fieldNumber
                            , bytesConsumed = bytesConsumed + n
                            , chunk = chunk
                            }
                        )
            )


chunkDecoder : WireType -> Decode.Decoder ( Int, Chunk )
chunkDecoder wireType =
    case wireType of
        VarInt ->
            Decode.map (Tuple.mapSecond <| Tuple.pair wireType) varIntBytesDecoder

        Bit64 ->
            Decode.map (Tuple.pair 8 << Tuple.pair wireType) (Decode.bytes 8)

        LengthDelimited length ->
            Decode.map (Tuple.pair length << Tuple.pair wireType) (Decode.bytes length)

        StartGroup ->
            Decode.fail

        EndGroup ->
            Decode.fail

        Bit32 ->
            Decode.map (Tuple.pair 4 << Tuple.pair wireType) (Decode.bytes 4)


tagDecoder : Decode.Decoder ( Int, ( Int, WireType ) )
tagDecoder =
    varIntDecoder
        |> Decode.andThen
            (\( bytesConsumed, value ) ->
                let
                    fieldNumber =
                        Bitwise.shiftRightZfBy 3 value
                in
                Decode.map (\( n, wireType ) -> ( bytesConsumed + n, ( fieldNumber, wireType ) )) <|
                    case Bitwise.and 0x07 value of
                        0 ->
                            Decode.succeed ( 0, VarInt )

                        1 ->
                            Decode.succeed ( 0, Bit64 )

                        2 ->
                            Decode.map (Tuple.mapSecond LengthDelimited) varIntDecoder

                        3 ->
                            Decode.succeed ( 0, StartGroup )

                        4 ->
                            Decode.succeed ( 0, EndGroup )

                        5 ->
                            Decode.succeed ( 0, Bit32 )

                        _ ->
                            Decode.fail
            )


lengthDelimitedDecoder : (Int -> Decode.Decoder a) -> Decoder a
lengthDelimitedDecoder decoder =
    Decoder
        (\wireType ->
            Decode.map (Tuple.mapFirst ConsumedBytes) <|
                case wireType of
                    LengthDelimited width ->
                        Decode.map (Tuple.pair width) (decoder width)

                    _ ->
                        Decode.fail
        )


packedDecoder : WireType -> Decode.Decoder ( Int, a ) -> Decoder a
packedDecoder decoderWireType decoder =
    Decoder
        (\wireType ->
            Decode.map (Tuple.mapFirst ConsumedBytes) <|
                case wireType of
                    LengthDelimited _ ->
                        decoder

                    _ ->
                        if wireType == decoderWireType then
                            decoder

                        else
                            Decode.fail
        )


stepPackedField : Int -> Decode.Decoder ( ConsumedBytes, a ) -> ( Int, List a ) -> Decode.Decoder (Decode.Step ( Int, List a ) ( ConsumedBytes, List a ))
stepPackedField fullWidth decoder ( width, values ) =
    decoder
        |> Decode.map
            (\( consumedBytes, value ) ->
                let
                    bytesRemaining =
                        case consumedBytes of
                            ConsumedBytes n ->
                                width - n

                            _ ->
                                width

                    values_ =
                        values ++ [ value ]
                in
                if bytesRemaining <= 0 then
                    Decode.Done ( ConsumedBytes fullWidth, values_ )

                else
                    Decode.Loop ( bytesRemaining, values_ )
            )



-- VARINT


varIntDecoder : Decode.Decoder ( Int, Int )
varIntDecoder =
    Decode.loop [] varIntStepDecoder
        |> Decode.map (Tuple.mapSecond (List.foldl (\octet value -> Bitwise.and 0x7F octet + Bitwise.shiftLeftBy 7 value) 0))


varIntBytesDecoder : Decode.Decoder ( Int, Bytes.Bytes )
varIntBytesDecoder =
    -- TODO find an alternative for decoding/encoding
    Decode.loop [] varIntStepDecoder
        |> Decode.map (Tuple.mapSecond (Encode.encode << Encode.sequence << List.map Encode.unsignedInt8 << List.reverse))


varIntStepDecoder : List Int -> Decode.Decoder (Decode.Step (List Int) ( Int, List Int ))
varIntStepDecoder state =
    Decode.unsignedInt8
        |> Decode.map
            (\octet ->
                let
                    newState =
                        octet :: state
                in
                if Bitwise.and 0x80 octet == 0x80 then
                    Decode.Loop newState

                else
                    Decode.Done ( List.length newState, newState )
            )


unsigned32 : Int -> Int
unsigned32 value =
    if value < 0 then
        value + 2 ^ 32

    else
        value


zigZag32 : Int -> Int
zigZag32 value =
    Bitwise.xor (Bitwise.shiftRightZfBy 1 value) (-1 * Bitwise.and 1 value)
