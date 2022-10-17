module Internal.Int64 exposing (Int32s, Int64, config, fromInt32s, toInt32s, zero)

import Bitwise
import Bytes exposing (Endianness(..))
import Internal.IntType as IntType


{-| Basic Int64 data type consisting of two Int32s.
To not bloat this package, only operations needed for en- and decoding are defined.
-}
type Int64
    = Int64 Int32s


type alias Int32s =
    { lower : Int, upper : Int }


{-| Construct an Int64 safely (making sure that the Ints are in the signed Int32 range)
-}
fromInt32s : Int32s -> Int64
fromInt32s =
    rewrap >> Int64


{-| Extract the Int32 values the Int64 data type consists of.
The `lower` and `upper` Int values are guaranteed to be within the signed Int32 range [-2^32 .. 2^31-1]
-}
toInt32s : Int64 -> Int32s
toInt32s (Int64 int32s) =
    int32s


{-| The constant 0
-}
zero : Int64
zero =
    Int64 { lower = 0, upper = 0 }


config : IntType.Config Int64
config =
    { zigZag = zigZag
    , zagZig = zagZig
    , fromUnsigned = identity
    , toUnsigned = identity
    , to7BitList = to7BitList
    , from7BitList = from7BitList
    }


zigZag : Int64 -> Int64
zigZag value =
    xor (shiftRightBy63 value)
        (shiftLeftBy 1 value)


zagZig : Int64 -> Int64
zagZig value =
    xor (shiftRightZfBy 1 value) (negate <| andInt 1 value)


to7BitList : Int64 -> List Int
to7BitList ((Int64 { lower }) as value) =
    let
        base128 =
            Bitwise.and 0x7F lower

        higherBits =
            shiftRightZfBy 7 value
    in
    if higherBits == zero then
        [ base128 ]

    else
        Bitwise.or 0x80 base128 :: to7BitList higherBits


from7BitList : List Int -> Int64
from7BitList =
    List.foldl (\sevenBitInt acc -> addUnsafe sevenBitInt (shiftLeftBy 7 acc)) zero


rewrap : Int32s -> Int32s
rewrap { lower, upper } =
    { lower = Bitwise.or 0 lower, upper = Bitwise.or 0 upper }


shiftRightZfBy : Int -> Int64 -> Int64
shiftRightZfBy n (Int64 int32s) =
    if n > 32 then
        fromInt32s { lower = Bitwise.shiftRightZfBy n int32s.upper, upper = 0 }

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
        fromInt32s { lower = newLower, upper = Bitwise.shiftRightZfBy n int32s.upper }


shiftRightBy63 : Int64 -> Int64
shiftRightBy63 (Int64 { upper }) =
    fromInt32s
        { lower = Bitwise.shiftRightBy 31 upper
        , upper =
            if upper >= 0 then
                0

            else
                -1
        }


andInt : Int -> Int64 -> Int64
andInt n (Int64 int32s) =
    fromInt32s { int32s | lower = Bitwise.and n int32s.lower, upper = 0 }


{-| Adds int to lower bits. Does NOT handle overflow!
-}
addUnsafe : Int -> Int64 -> Int64
addUnsafe n (Int64 int32s) =
    fromInt32s { int32s | lower = n + int32s.lower }


negate : Int64 -> Int64
negate ((Int64 { lower, upper }) as value) =
    if value == zero then
        zero

    else
        Int64 { lower = Bitwise.complement lower + 1, upper = Bitwise.complement upper }


shiftLeftBy : Int -> Int64 -> Int64
shiftLeftBy n (Int64 { lower, upper }) =
    if n > 32 then
        fromInt32s { lower = 0, upper = Bitwise.shiftLeftBy n lower }

    else
        let
            carry =
                Bitwise.shiftRightZfBy (32 - n) lower

            newUpper =
                upper
                    |> Bitwise.shiftLeftBy n
                    |> Bitwise.or carry
        in
        fromInt32s { lower = Bitwise.shiftLeftBy n lower, upper = newUpper }


xor : Int64 -> Int64 -> Int64
xor (Int64 i) (Int64 j) =
    fromInt32s
        { lower = Bitwise.xor i.lower j.lower
        , upper = Bitwise.xor i.upper j.upper
        }
