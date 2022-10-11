module Internal.Protobuf exposing (GenericInt, WireType(..), generic32, generic64)

import Bitwise
import Bytes exposing (Endianness(..))
import Int64 exposing (Int64)
import List
import Maybe


type WireType
    = VarInt
    | Bit64
    | LengthDelimited Int
    | StartGroup
    | EndGroup
    | Bit32


type alias GenericInt a =
    { fromInt : Int -> a
    , lastByte : a -> Int
    , and : a -> a -> a
    , or : a -> a -> a
    , xor : a -> a -> a
    , shiftRightZfBy : Int -> a -> a
    , shiftRightBy : Int -> a -> a
    }


generic32 : GenericInt Int
generic32 =
    { fromInt = identity
    , lastByte = Bitwise.and 0xFF
    , and = Bitwise.and
    , or = Bitwise.or
    , xor = Bitwise.xor
    , shiftRightZfBy = Bitwise.shiftRightZfBy
    , shiftRightBy = Bitwise.shiftRightBy
    }


generic64 : GenericInt Int64
generic64 =
    { fromInt = Int64.fromInt
    , lastByte = Maybe.withDefault 0 << List.head << List.reverse << Int64.toByteValues
    , and = Int64.and
    , or = Int64.or
    , xor = Int64.xor
    , shiftRightZfBy = Int64.shiftRightZfBy
    , shiftRightBy = Int64.shiftRightZfBy -- FIXME
    }
