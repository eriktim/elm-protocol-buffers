module Internal.Int32 exposing (operations)

import Bitwise
import Internal.IntOperations exposing (IntOperations)


operations : IntOperations Int
operations =
    { zigZag = zigZag
    , zagZig = zagZig
    , fromUnsigned = fromUnsigned
    , toUnsigned = toUnsigned
    , pushBase128 = pushByte
    , popBase128 = popByte
    , fromBase128 = identity
    }


zigZag : Int -> Int
zigZag value =
    Bitwise.xor (Bitwise.shiftRightBy 31 value) (Bitwise.shiftLeftBy 1 value)


zagZig : Int -> Int
zagZig value =
    Bitwise.xor (Bitwise.shiftRightZfBy 1 value) (-1 * Bitwise.and 1 value)


fromUnsigned : Int -> Int
fromUnsigned value =
    if value >= 2 ^ 31 then
        value - 2 ^ 32

    else
        value


toUnsigned : Int -> Int
toUnsigned value =
    if value < 0 then
        value + 2 ^ 32

    else
        value


popByte : Int -> ( Int, Int )
popByte value =
    let
        base128 =
            Bitwise.and 0x7F value

        higherBits =
            Bitwise.shiftRightZfBy 7 value
    in
    ( base128, higherBits )


pushByte : Int -> Int -> Int
pushByte sevenBitInt acc =
    sevenBitInt + Bitwise.shiftLeftBy 7 acc
