module ProtoBuf.Encode exposing
    ( Encoder
    , FieldEncoder
    , bool
    , bytes
    , double
    , embedded
    , encode
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
import Bytes.Encode as Encode
import Internal.ProtoBuf exposing (WireType(..))


type Encoder
    = Encoder Encode.Encoder


type
    FieldEncoder
    -- TODO merge these two (if possible)
    = FieldEncoder WireType Encode.Encoder
    | RepeatedEncoder WireType (List Encode.Encoder)


encode : Encoder -> Bytes
encode (Encoder encoder) =
    Encode.encode encoder


message : List ( Int, FieldEncoder ) -> Encoder
message items =
    uniqueByFieldNumber items
        |> List.sortBy Tuple.first
        |> List.map
            (\( fieldNumber, encoder ) ->
                case encoder of
                    FieldEncoder wireType encoder_ ->
                        let
                            bs =
                                Encode.encode encoder_

                            length =
                                case wireType of
                                    LengthDelimited ->
                                        varIntEncode (Bytes.width bs)

                                    _ ->
                                        Encode.sequence []
                        in
                        Encode.sequence
                            [ tagEncoder fieldNumber wireType
                            , length
                            , Encode.bytes bs
                            ]

                    RepeatedEncoder wireType encoders ->
                        -- TODO mix w/ previous OR (probably) move to #repeated
                        if wireType == LengthDelimited then
                            Encode.sequence <|
                                List.concatMap
                                    (\encoder_ ->
                                        let
                                            bs =
                                                Encode.encode encoder_
                                        in
                                        [ tagEncoder fieldNumber wireType
                                        , varIntEncode (Bytes.width bs)
                                        , Encode.bytes bs
                                        ]
                                    )
                                    encoders

                        else
                            let
                                bs =
                                    Encode.encode <| Encode.sequence encoders
                            in
                            Encode.sequence
                                [ tagEncoder fieldNumber LengthDelimited
                                , varIntEncode (Bytes.width bs)
                                , Encode.bytes bs
                                ]
            )
        |> Encode.sequence
        |> Encoder


repeated : (a -> FieldEncoder) -> List a -> FieldEncoder
repeated encode_ values =
    let
        encoders =
            List.map encode_ values
                |> List.filterMap
                    (\encoder ->
                        case encoder of
                            FieldEncoder wireType encoder_ ->
                                Just ( wireType, encoder_ )

                            RepeatedEncoder _ _ ->
                                Nothing
                    )

        encoders_ =
            List.map Tuple.second encoders
    in
    case encoders of
        ( wireType, _ ) :: _ ->
            RepeatedEncoder wireType encoders_

        _ ->
            RepeatedEncoder LengthDelimited []


embedded : (a -> Encoder) -> Maybe a -> FieldEncoder
embedded encoder value =
    case Maybe.map encoder value of
        Just (Encoder encoder_) ->
            FieldEncoder LengthDelimited encoder_

        Nothing ->
            RepeatedEncoder LengthDelimited []


double : Float -> FieldEncoder
double value =
    FieldEncoder Bit64 (Encode.float64 LE value)


float : Float -> FieldEncoder
float value =
    FieldEncoder Bit32 (Encode.float32 LE value)


int32 : Int -> FieldEncoder
int32 value =
    FieldEncoder VarInt (varIntEncode value)


uint32 : Int -> FieldEncoder
uint32 =
    FieldEncoder VarInt << varIntEncode << unsignedEncode


sint32 : Int -> FieldEncoder
sint32 =
    FieldEncoder VarInt << varIntEncode << zigZagEncode


fixed32 : Int -> FieldEncoder
fixed32 value =
    FieldEncoder Bit32 (Encode.unsignedInt32 LE value)


sfixed32 : Int -> FieldEncoder
sfixed32 value =
    FieldEncoder Bit32 (Encode.signedInt32 LE value)


bool : Bool -> FieldEncoder
bool value =
    let
        num =
            if value then
                1

            else
                0
    in
    FieldEncoder VarInt (varIntEncode num)


string : String -> FieldEncoder
string =
    FieldEncoder LengthDelimited << Encode.string


bytes : Bytes -> FieldEncoder
bytes =
    FieldEncoder LengthDelimited << Encode.bytes



--enum ...
--oneof ...
--map ...
-- HELPERS


tagEncoder : Int -> WireType -> Encode.Encoder
tagEncoder fieldNumber wireType =
    let
        base4 =
            case wireType of
                VarInt ->
                    0

                Bit64 ->
                    1

                LengthDelimited ->
                    2

                StartGroup ->
                    3

                EndGroup ->
                    4

                Bit32 ->
                    5
    in
    Bitwise.or (Bitwise.shiftLeftBy 3 fieldNumber) base4
        |> varIntEncode


varIntEncode : Int -> Encode.Encoder
varIntEncode value =
    Encode.sequence (varIntEncoders value)


varIntEncoders : Int -> List Encode.Encoder
varIntEncoders value =
    let
        base128 =
            Bitwise.and 0x7F value

        higherBits =
            Bitwise.shiftRightZfBy 7 value
    in
    if higherBits /= 0x00 then
        Encode.unsignedInt8 (Bitwise.or 0x80 base128) :: varIntEncoders higherBits

    else
        [ Encode.unsignedInt8 base128 ]


unsignedEncode : Int -> Int
unsignedEncode value =
    if value >= 2 ^ 31 then
        value - 2 ^ 32

    else
        value


zigZagEncode : Int -> Int
zigZagEncode value =
    Bitwise.xor (Bitwise.shiftRightBy 31 value) (Bitwise.shiftLeftBy 1 value)


uniqueByFieldNumber : List ( Int, FieldEncoder ) -> List ( Int, FieldEncoder )
uniqueByFieldNumber items =
    case items of
        x :: xs ->
            if List.member (Tuple.first x) (List.map Tuple.first xs) then
                uniqueByFieldNumber xs

            else
                x :: uniqueByFieldNumber xs

        [] ->
            []
