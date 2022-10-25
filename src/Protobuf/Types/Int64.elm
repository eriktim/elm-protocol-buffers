module Protobuf.Types.Int64 exposing
    ( Int64, Int32s
    , fromInt32s, toInt32s
    )

{-| A simple 64-bit integer made up from two 32-bit integers.
This is used by the `int64` and related en/decoders.
This module is intentionally kept sparse to achieve the following goals:

  - Support Int64 data types in elm-protocol-buffers
  - Don't add extra code or dependencies not required for the main goal of this package (serialization and deserialization)

If you find yourself needing extra logic/arithmetic revolving around 64-bit integers,
please use one of the available packages providing what you need and convert with
`fromInt32s` and `toInt32s` between the two data types.


# Data Types

@docs Int64, Int32s


# Conversions

@docs fromInt32s, toInt32s

-}

import Bitwise


{-| The `Int64` data type. Guarantees the invariant that the internal integers are kept between `-2 ^ 31` and `2 ^ 31 - 1`.
-}
type Int64
    = Int64 Int32s


{-| A record containing two `Int` values, one for the lower 32 bits and one for the upper 32 bits.
-}
type alias Int32s =
    { lower : Int, upper : Int }


{-| Build an `Int64` from two `Int` values.
-}
fromInt32s : Int32s -> Int64
fromInt32s =
    rewrap >> Int64


{-| Get the two `Int` values for lower and upper bits from an `Int64`.
-}
toInt32s : Int64 -> Int32s
toInt32s (Int64 int32s) =
    int32s


rewrap : Int32s -> Int32s
rewrap { lower, upper } =
    { lower = Bitwise.or 0 lower, upper = Bitwise.or 0 upper }
