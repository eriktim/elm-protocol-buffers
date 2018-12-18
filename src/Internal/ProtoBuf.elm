module Internal.ProtoBuf exposing (WireType(..))


type WireType
    = VarInt
    | Bit64
    | LengthDelimited
    | StartGroup
    | EndGroup
    | Bit32
