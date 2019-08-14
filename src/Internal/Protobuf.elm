module Internal.Protobuf exposing (Chunk, FieldData, Message, WireType(..), emptyBytes)

import Bytes
import Bytes.Encode


type alias Message a =
    ( FieldData, a )


type alias FieldData =
    List ( Int, Chunk )


type alias Chunk =
    ( WireType, Bytes.Bytes )


type WireType
    = VarInt
    | Bit64
    | LengthDelimited Int
    | StartGroup
    | EndGroup
    | Bit32


emptyBytes : Bytes.Bytes
emptyBytes =
    Bytes.Encode.encode (Bytes.Encode.sequence [])
