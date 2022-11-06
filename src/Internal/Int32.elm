module Internal.Int32 exposing (operations)

import Bitwise
import Internal.IntOperations exposing (IntOperations)


operations : IntOperations Int
operations =
    { toZigZag = toZigZag
    , fromZigZag = fromZigZag
    , toSigned = toSigned
    , fromSigned = fromSigned
    , popBase128 = popBase128
    , pushBase128 = pushBase128
    , fromBase128 = identity
    }


toZigZag : Int -> Int
toZigZag value =
    Bitwise.xor (Bitwise.shiftRightBy 31 value) (Bitwise.shiftLeftBy 1 value)


fromZigZag : Int -> Int
fromZigZag value =
    Bitwise.xor (Bitwise.shiftRightZfBy 1 value) (-1 * Bitwise.and 1 value)


toSigned : Int -> Int
toSigned value =
    if value >= 2 ^ 31 then
        value - 2 ^ 32

    else
        value


fromSigned : Int -> Int
fromSigned value =
    if value < 0 then
        value + 2 ^ 32

    else
        value


popBase128 : Int -> ( Int, Int )
popBase128 value =
    let
        base128 =
            Bitwise.and 0x7F value

        higherBits =
            Bitwise.shiftRightZfBy 7 value
    in
    ( base128, higherBits )


pushBase128 : Int -> Int -> Int
pushBase128 base128 int =
    base128 + Bitwise.shiftLeftBy 7 int
