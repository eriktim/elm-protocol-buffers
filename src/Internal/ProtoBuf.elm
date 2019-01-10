module Internal.ProtoBuf exposing (WireType(..))


type WireType
    = VarInt
    | Bit64
    | LengthDelimited Int
    | StartGroup
    | EndGroup
    | Bit32
