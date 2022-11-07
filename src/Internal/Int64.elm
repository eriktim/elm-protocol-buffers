module Internal.Int64 exposing (Int64, fromInts, operations, toInts)

import Bitwise
import Internal.IntOperations exposing (IntOperations)


type Int64
    = Int64 Ints


type alias Ints =
    { higher : Int, lower : Int }


fromInts : Int -> Int -> Int64
fromInts higher lower =
    Int64 { higher = Bitwise.or 0 higher, lower = Bitwise.or 0 lower }


toInts : Int64 -> ( Int, Int )
toInts (Int64 { higher, lower }) =
    ( higher, lower )


operations : IntOperations Int64
operations =
    { toZigZag = toZigZag
    , fromZigZag = fromZigZag
    , toSigned = identity
    , fromSigned = identity
    , popBase128 = popBase128
    , pushBase128 = pushBase128
    , fromBase128 = fromBase128
    }


toZigZag : Int64 -> Int64
toZigZag value =
    xor (shiftRightBy63 value) (shiftLeftBy 1 value)


fromZigZag : Int64 -> Int64
fromZigZag value =
    xor (shiftRightZfBy 1 value) (value |> and 1 |> negate)


popBase128 : Int64 -> ( Int, Int64 )
popBase128 ((Int64 { lower }) as int) =
    let
        base128 =
            Bitwise.and 0x7F lower

        higherBits =
            shiftRightZfBy 7 int
    in
    ( base128, higherBits )


pushBase128 : Int -> Int64 -> Int64
pushBase128 base128 int =
    addUnsafe base128 (shiftLeftBy 7 int)


fromBase128 : Int -> Int64
fromBase128 =
    fromInts 0



-- HELPERS


shiftRightZfBy : Int -> Int64 -> Int64
shiftRightZfBy n (Int64 { higher, lower }) =
    if n > 32 then
        fromInts 0 (Bitwise.shiftRightZfBy n higher)

    else
        let
            carry =
                Bitwise.shiftLeftBy (32 - n) higher

            newLower =
                lower
                    |> Bitwise.shiftRightZfBy n
                    |> Bitwise.or carry
                    |> Bitwise.shiftRightZfBy 0
        in
        fromInts (Bitwise.shiftRightZfBy n higher) newLower


shiftRightBy63 : Int64 -> Int64
shiftRightBy63 (Int64 { higher }) =
    let
        onlyOnesOrZeros =
            Bitwise.shiftRightBy 31 higher
    in
    fromInts onlyOnesOrZeros onlyOnesOrZeros


and : Int -> Int64 -> Int64
and n (Int64 { lower }) =
    fromInts 0 (Bitwise.and n lower)


addUnsafe : Int -> Int64 -> Int64
addUnsafe n (Int64 { higher, lower }) =
    -- ignore possible overflow
    fromInts higher (n + lower)


negate : Int64 -> Int64
negate ((Int64 { higher, lower }) as int) =
    if lower == 0 && higher == 0 then
        int

    else
        fromInts (Bitwise.complement higher) (Bitwise.complement lower + 1)


shiftLeftBy : Int -> Int64 -> Int64
shiftLeftBy n (Int64 { higher, lower }) =
    if n > 32 then
        fromInts (Bitwise.shiftLeftBy n lower) 0

    else
        let
            carry =
                Bitwise.shiftRightZfBy (32 - n) lower

            newHigher =
                higher
                    |> Bitwise.shiftLeftBy n
                    |> Bitwise.or carry
        in
        fromInts newHigher (Bitwise.shiftLeftBy n lower)


xor : Int64 -> Int64 -> Int64
xor (Int64 a) (Int64 b) =
    fromInts
        (Bitwise.xor a.higher b.higher)
        (Bitwise.xor a.lower b.lower)
