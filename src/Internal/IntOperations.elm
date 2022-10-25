module Internal.IntOperations exposing (IntOperations, int32Operations, int64Operations)

import Bitwise
import Protobuf.Types.Int64 as Int64 exposing (Int64)


type alias IntOperations int =
    { zigZag : int -> int
    , zagZig : int -> int
    , toUnsigned : int -> int
    , fromUnsigned : int -> int
    , split7Bit : int -> ( Int, int )
    , add7Bit : Int -> int -> int
    , zero : int
    }


int32Operations : IntOperations Int
int32Operations =
    { zigZag = zigZagInt32
    , zagZig = zagZigInt32
    , fromUnsigned = fromUnsignedInt32
    , toUnsigned = toUnsignedInt32
    , add7Bit = add7BitInt32
    , split7Bit = split7BitInt32
    , zero = 0
    }


int64Operations : IntOperations Int64
int64Operations =
    { zigZag = zigZag
    , zagZig = zagZig
    , fromUnsigned = identity
    , toUnsigned = identity
    , split7Bit = to7BitList
    , add7Bit = from7BitList
    , zero = zero
    }



-- Int32 Implementations


zigZagInt32 : Int -> Int
zigZagInt32 value =
    Bitwise.xor (Bitwise.shiftRightBy 31 value) (Bitwise.shiftLeftBy 1 value)


zagZigInt32 : Int -> Int
zagZigInt32 value =
    Bitwise.xor (Bitwise.shiftRightZfBy 1 value) (-1 * Bitwise.and 1 value)


fromUnsignedInt32 : Int -> Int
fromUnsignedInt32 value =
    if value >= 2 ^ 31 then
        value - 2 ^ 32

    else
        value


toUnsignedInt32 : Int -> Int
toUnsignedInt32 value =
    if value < 0 then
        value + 2 ^ 32

    else
        value


split7BitInt32 : Int -> ( Int, Int )
split7BitInt32 value =
    let
        base128 =
            Bitwise.and 0x7F value

        higherBits =
            Bitwise.shiftRightZfBy 7 value
    in
    ( base128, higherBits )


add7BitInt32 : Int -> Int -> Int
add7BitInt32 sevenBitInt acc =
    sevenBitInt + Bitwise.shiftLeftBy 7 acc



-- Int64 Implementations


zigZag : Int64 -> Int64
zigZag value =
    xor (shiftRightBy63 value)
        (shiftLeftBy 1 value)


zagZig : Int64 -> Int64
zagZig value =
    xor (shiftRightZfBy 1 value) (value |> andInt 1 |> negate)


to7BitList : Int64 -> ( Int, Int64 )
to7BitList int64 =
    let
        { lower } =
            Int64.toInt32s int64

        base128 =
            Bitwise.and 0x7F lower

        higherBits =
            shiftRightZfBy 7 int64
    in
    ( base128, higherBits )


from7BitList : Int -> Int64 -> Int64
from7BitList sevenBitInt acc =
    addUnsafe sevenBitInt (shiftLeftBy 7 acc)


shiftRightZfBy : Int -> Int64 -> Int64
shiftRightZfBy n int64 =
    let
        int32s =
            Int64.toInt32s int64
    in
    if n > 32 then
        Int64.fromInt32s { lower = Bitwise.shiftRightZfBy n int32s.upper, upper = 0 }

    else
        let
            carry =
                Bitwise.shiftLeftBy (32 - n) int32s.upper

            newLower =
                int32s.lower
                    |> Bitwise.shiftRightZfBy n
                    |> Bitwise.or carry
                    |> Bitwise.shiftRightZfBy 0
        in
        Int64.fromInt32s { lower = newLower, upper = Bitwise.shiftRightZfBy n int32s.upper }


shiftRightBy63 : Int64 -> Int64
shiftRightBy63 int64 =
    let
        { upper } =
            Int64.toInt32s int64
    in
    Int64.fromInt32s
        { lower = Bitwise.shiftRightBy 31 upper
        , upper =
            if upper >= 0 then
                0

            else
                -1
        }


andInt : Int -> Int64 -> Int64
andInt n int64 =
    let
        int32s =
            Int64.toInt32s int64
    in
    Int64.fromInt32s { int32s | lower = Bitwise.and n int32s.lower, upper = 0 }


{-| Adds int to lower bits. Does NOT handle overflow!
-}
addUnsafe : Int -> Int64 -> Int64
addUnsafe n int64 =
    let
        int32s =
            Int64.toInt32s int64
    in
    Int64.fromInt32s { int32s | lower = n + int32s.lower }


negate : Int64 -> Int64
negate int64 =
    let
        { lower, upper } =
            Int64.toInt32s int64
    in
    if int64 == zero then
        zero

    else
        Int64.fromInt32s { lower = Bitwise.complement lower + 1, upper = Bitwise.complement upper }


shiftLeftBy : Int -> Int64 -> Int64
shiftLeftBy n int64 =
    let
        { lower, upper } =
            Int64.toInt32s int64
    in
    if n > 32 then
        Int64.fromInt32s { lower = 0, upper = Bitwise.shiftLeftBy n lower }

    else
        let
            carry =
                Bitwise.shiftRightZfBy (32 - n) lower

            newUpper =
                upper
                    |> Bitwise.shiftLeftBy n
                    |> Bitwise.or carry
        in
        Int64.fromInt32s { lower = Bitwise.shiftLeftBy n lower, upper = newUpper }


xor : Int64 -> Int64 -> Int64
xor a b =
    let
        i =
            Int64.toInt32s a

        j =
            Int64.toInt32s b
    in
    Int64.fromInt32s
        { lower = Bitwise.xor i.lower j.lower
        , upper = Bitwise.xor i.upper j.upper
        }


zero : Int64
zero =
    Int64.fromInt32s { lower = 0, upper = 0 }
