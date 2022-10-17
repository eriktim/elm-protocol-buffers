module Protobuf.Types.Int64 exposing (Int32s, Int64, fromInt32s, toInt32s)

import Internal.Int64


type alias Int64 =
    Internal.Int64.Int64


type alias Int32s =
    Internal.Int64.Int32s


fromInt32s : Int32s -> Int64
fromInt32s =
    Internal.Int64.fromInt32s


toInt32s : Int64 -> Int32s
toInt32s =
    Internal.Int64.toInt32s
