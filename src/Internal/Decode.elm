module Internal.Decode exposing
    ( Decoder
    , bool
    , bytes
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
import Http
import Internal.Protobuf exposing (..)
import Set



-- DECODER


type Decoder a
    = Decoder (WireType -> Decode.Decoder ( Int, a ))



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
                        |> Decode.map (\fieldData -> ( width, ( fieldData, v ) ))

                _ ->
                    Decode.fail
        )



-- FIELD DECODERS


decodeField : (Int -> Bool) -> (FieldData -> Maybe v) -> Decoder ( FieldData, v -> a ) -> Decoder ( FieldData, a )
decodeField predicate decodeFields (Decoder d) =
    Decoder
        (\wireType ->
            d wireType
                |> Decode.andThen
                    (\( usedBytes, ( fieldData, f ) ) ->
                        let
                            ( fields, newFieldData ) =
                                List.partition (predicate << Tuple.first) fieldData
                        in
                        case decodeFields fields of
                            Just value ->
                                Decode.succeed ( 0, ( newFieldData, f value ) )

                            Nothing ->
                                Decode.fail
                    )
        )


field : Int -> Decoder v -> Maybe v -> Decoder ( FieldData, v -> a ) -> Decoder ( FieldData, a )
field fieldNumber decoder default =
    decodeField
        ((==) fieldNumber)
        (\fields ->
            let
                decodedValue =
                    List.head fields
                        |> Maybe.andThen (decodeChunk decoder << Tuple.second)
            in
            case decodedValue of
                Nothing ->
                    default

                v ->
                    v
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
    decodeField
        ((==) fieldNumber)
        (\fields ->
            List.map Tuple.second fields
                |> List.foldr
                    (\chunk values ->
                        Maybe.map2 (++) values (decodeChunk listDecoder chunk)
                    )
                    (Just [])
        )


dict : Int -> Decoder comparable -> Decoder v -> ( Maybe comparable, Maybe v ) -> Decoder ( FieldData, Dict.Dict comparable v -> a ) -> Decoder ( FieldData, a )
dict fieldNumber keyDecoder valueDecoder ( defaultKey, defaultValue ) =
    let
        dictDecoder =
            message Tuple.pair
                |> field 1 keyDecoder defaultKey
                |> field 2 valueDecoder defaultValue
    in
    decodeField
        ((==) fieldNumber)
        (\fields ->
            List.map Tuple.second fields
                |> List.foldr
                    (\chunk values ->
                        Maybe.map2 (++) values (Maybe.map (List.singleton << Tuple.second) <| decodeChunk dictDecoder chunk)
                    )
                    (Just [])
                |> Maybe.map Dict.fromList
        )


oneOf : List ( Int, Decoder v ) -> Decoder ( FieldData, Maybe v -> a ) -> Decoder ( FieldData, a )
oneOf decoders =
    let
        fieldNumbers =
            List.map Tuple.first decoders
    in
    decodeField
        (\n -> List.member n fieldNumbers)
        (\fields ->
            case fields of
                ( fieldNumber, chunk ) :: _ ->
                    case List.head (List.filter ((==) fieldNumber << Tuple.first) decoders) of
                        Just ( _, decoder ) ->
                            Just (decodeChunk decoder chunk)

                        Nothing ->
                            Nothing

                [] ->
                    Just Nothing
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
        fieldDecoder
            |> Decode.map
                (\{ fieldNumber, usedBytes, chunk } ->
                    Decode.Loop ( bytesRemaining - usedBytes, ( fieldNumber, chunk ) :: fieldData )
                )


fieldDecoder : Decode.Decoder { fieldNumber : Int, usedBytes : Int, chunk : Chunk }
fieldDecoder =
    tagDecoder
        |> Decode.andThen
            (\( usedBytes, ( fieldNumber, wireType ) ) ->
                chunkDecoder wireType
                    |> Decode.map
                        (\( n, chunk ) ->
                            { fieldNumber = fieldNumber
                            , usedBytes = usedBytes + n
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


decodeChunk : Decoder a -> Chunk -> Maybe a
decodeChunk (Decoder decoder) ( wireType, bs ) =
    Decode.decode (decoder wireType) bs
        |> Maybe.map Tuple.second


tagDecoder : Decode.Decoder ( Int, ( Int, WireType ) )
tagDecoder =
    varIntDecoder
        |> Decode.andThen
            (\( usedBytes, value ) ->
                let
                    fieldNumber =
                        Bitwise.shiftRightZfBy 3 value
                in
                Decode.map (\( n, wireType ) -> ( usedBytes + n, ( fieldNumber, wireType ) )) <|
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
            case wireType of
                LengthDelimited _ ->
                    decoder

                _ ->
                    if wireType == decoderWireType then
                        decoder

                    else
                        Decode.fail
        )


stepPackedField : Int -> Decode.Decoder ( Int, a ) -> ( Int, List a ) -> Decode.Decoder (Decode.Step ( Int, List a ) ( Int, List a ))
stepPackedField fullWidth decoder ( width, values ) =
    decoder
        |> Decode.map
            (\( w, value ) ->
                let
                    bytesRemaining =
                        width - w

                    values_ =
                        values ++ [ value ]
                in
                if bytesRemaining <= 0 then
                    Decode.Done ( fullWidth, values_ )

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
