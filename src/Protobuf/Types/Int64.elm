module Protobuf.Types.Int64 exposing
    ( Int64
    , fromInts, toInts
    )

{-| A simple 64-bit integer made up from two 32-bit integers.
This is used by the `int64` and related en/decoders.
This module is intentionally kept sparse to achieve the following goals:

  - Support Int64 data types in elm-protocol-buffers
  - Don't add extra code or dependencies not required for the main goal of this package (serialization and deserialization)

If you find yourself needing extra logic/arithmetic revolving around 64-bit integers,
please use one of the available packages providing what you need and convert with
`fromInts` and `toInts` between the two data types.


# Data Types

@docs Int64


# Conversions

@docs fromInts, toInts

-}

import Internal.Int64


{-| The `Int64` data type. Guarantees the invariant that the internal integers are kept between `-2 ^ 31` and `2 ^ 31 - 1`.
-}
type alias Int64 =
    Internal.Int64.Int64


{-| Build an `Int64` from two `Int` values.
-}
fromInts : Int -> Int -> Int64
fromInts =
    Internal.Int64.fromInts


{-| Get the two `Int` values for higher and lower bits from an `Int64`.
-}
toInts : Int64 -> ( Int, Int )
toInts =
    Internal.Int64.toInts
