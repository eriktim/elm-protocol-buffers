module Internal.Encode exposing
    ( Encoder
    , FieldEncoder
    , Packing(..)
    , bool
    , bytes
    , dict
    , double
    , encode
    , fixed32
    , float
    , int32
    , list
    , message
    , none
    , sfixed32
    , sint32
    , string
    , uint32
    )

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode
import Dict exposing (Dict)
import Internal.Protobuf exposing (..)



-- ENCODER


type Encoder
    = Encoder WireType ( Int, Encode.Encoder )
    | ListEncoder Packing (List Encoder)
    | EmptyEncoder


type alias FieldEncoder =
    ( Int, Encoder )


type Packing
    = AutoPacking
    | NoPacking



-- ENCODE


encode : Encoder -> Bytes
encode encoder =
    case encoder of
        Encoder _ ( _, encoder_ ) ->
            Encode.encode encoder_

        ListEncoder _ encoders ->
            List.map (Encode.bytes << encode) encoders
                |> Encode.sequence
                |> Encode.encode

        EmptyEncoder ->
            emptyBytes


message : FieldData -> List FieldEncoder -> Encoder
message fieldData items =
    items
        |> List.sortBy Tuple.first
        |> List.map toKeyValuePairEncoder
        |> (\encoders -> encoders ++ List.map (\( num, ( _, bs ) ) -> toKeyValuePairEncoder ( num, bytes bs )) fieldData)
        |> sequence
        |> (\e -> Encoder (LengthDelimited (Tuple.first e)) e)



-- PRIMITIVES


int32 : Int -> Encoder
int32 =
    Encoder VarInt << varInt


uint32 : Int -> Encoder
uint32 =
    Encoder VarInt << varInt << unsigned32


sint32 : Int -> Encoder
sint32 =
    Encoder VarInt << varInt << zigZag32


fixed32 : Int -> Encoder
fixed32 =
    Encoder Bit32 << Tuple.pair 4 << Encode.unsignedInt32 LE


sfixed32 : Int -> Encoder
sfixed32 =
    Encoder Bit32 << Tuple.pair 4 << Encode.signedInt32 LE


double : Float -> Encoder
double =
    Encoder Bit64 << Tuple.pair 8 << Encode.float64 LE


float : Float -> Encoder
float =
    Encoder Bit32 << Tuple.pair 4 << Encode.float32 LE


string : String -> Encoder
string v =
    let
        width =
            Encode.getStringWidth v
    in
    Encoder (LengthDelimited width) ( width, Encode.string v )


bool : Bool -> Encoder
bool v =
    Encoder VarInt <|
        if v then
            varInt 1

        else
            varInt 0


bytes : Bytes -> Encoder
bytes v =
    let
        width =
            Bytes.width v
    in
    Encoder (LengthDelimited width) ( width, Encode.bytes v )


none : Encoder
none =
    EmptyEncoder



-- DATA STRUCTURES


list : Packing -> (v -> Encoder) -> List v -> Encoder
list packing fn =
    ListEncoder packing << List.map fn


dict : (k -> Encoder) -> (v -> Encoder) -> Dict k v -> Encoder
dict encodeKey encodeValue kvs =
    Dict.toList kvs
        |> List.map
            (\( key, value ) ->
                message []
                    [ ( 1, encodeKey key )
                    , ( 2, encodeValue value )
                    ]
            )
        |> ListEncoder NoPacking



-- BYTES ENCODER


sequence : List ( Int, Encode.Encoder ) -> ( Int, Encode.Encoder )
sequence items =
    let
        width =
            List.sum <| List.map Tuple.first items
    in
    ( width, Encode.sequence <| List.map Tuple.second items )


toKeyValuePairEncoder : ( Int, Encoder ) -> ( Int, Encode.Encoder )
toKeyValuePairEncoder ( fieldNumber, encoder ) =
    case encoder of
        Encoder wireType encoder_ ->
            sequence
                [ tag fieldNumber wireType
                , encoder_
                ]

        ListEncoder packing encoders ->
            let
                packedEncoder =
                    case packing of
                        AutoPacking ->
                            toPackedEncoder encoders

                        NoPacking ->
                            Nothing
            in
            case packedEncoder of
                Just encoder_ ->
                    sequence
                        [ tag fieldNumber (LengthDelimited (Tuple.first encoder_))
                        , encoder_
                        ]

                _ ->
                    sequence <| List.map (toKeyValuePairEncoder << Tuple.pair fieldNumber) encoders

        EmptyEncoder ->
            sequence []


toPackedEncoder : List Encoder -> Maybe ( Int, Encode.Encoder )
toPackedEncoder encoders =
    case encoders of
        (Encoder wireType encoder) :: others ->
            case wireType of
                LengthDelimited _ ->
                    Nothing

                _ ->
                    Just <| sequence (encoder :: List.filterMap unwrap others)

        _ ->
            Nothing


unwrap : Encoder -> Maybe ( Int, Encode.Encoder )
unwrap encoder =
    case encoder of
        Encoder _ encoder_ ->
            Just encoder_

        _ ->
            Nothing


tag : Int -> WireType -> ( Int, Encode.Encoder )
tag fieldNumber wireType =
    let
        encodeTag base4 =
            varInt (Bitwise.or (Bitwise.shiftLeftBy 3 fieldNumber) base4)
    in
    case wireType of
        VarInt ->
            encodeTag 0

        Bit64 ->
            encodeTag 1

        LengthDelimited width ->
            sequence [ encodeTag 2, varInt width ]

        StartGroup ->
            encodeTag 3

        EndGroup ->
            encodeTag 4

        Bit32 ->
            encodeTag 5



-- VARINT


varInt : Int -> ( Int, Encode.Encoder )
varInt value =
    let
        encoders =
            toVarIntEncoders value
    in
    ( List.length encoders, Encode.sequence encoders )


unsigned32 : Int -> Int
unsigned32 value =
    if value >= 2 ^ 31 then
        value - 2 ^ 32

    else
        value


zigZag32 : Int -> Int
zigZag32 value =
    Bitwise.xor (Bitwise.shiftRightBy 31 value) (Bitwise.shiftLeftBy 1 value)


toVarIntEncoders : Int -> List Encode.Encoder
toVarIntEncoders value =
    let
        base128 =
            Bitwise.and 0x7F value

        higherBits =
            Bitwise.shiftRightZfBy 7 value
    in
    if higherBits /= 0x00 then
        Encode.unsignedInt8 (Bitwise.or 0x80 base128) :: toVarIntEncoders higherBits

    else
        [ Encode.unsignedInt8 base128 ]
