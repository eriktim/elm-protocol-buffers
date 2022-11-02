module Internal.IntOperations exposing (IntOperations)


type alias IntOperations int =
    { zigZag : int -> int
    , zagZig : int -> int
    , toUnsigned : int -> int
    , fromUnsigned : int -> int
    , popBase128 : int -> ( Int, int )
    , pushBase128 : Int -> int -> int
    , fromBase128 : Int -> int
    }
