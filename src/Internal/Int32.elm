module Internal.Int32 exposing (config)

import Bitwise
import Internal.IntType as IntType


config : IntType.Config Int
config =
    { zigZag = zigZag
    , zagZig = zagZig
    , fromUnsigned = fromUnsigned
    , toUnsigned = toUnsigned
    , from7BitList = from7BitList
    , to7BitList = to7BitList
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


to7BitList : Int -> List Int
to7BitList value =
    let
        base128 =
            Bitwise.and 0x7F value

        higherBits =
            Bitwise.shiftRightZfBy 7 value
    in
    if higherBits == 0 then
        [ base128 ]

    else
        Bitwise.or 0x80 base128 :: to7BitList higherBits


from7BitList : List Int -> Int
from7BitList =
    List.foldl (\sevenBitInt acc -> sevenBitInt + Bitwise.shiftLeftBy 7 acc) 0
