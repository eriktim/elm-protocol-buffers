module Internal.IntOperations exposing (IntOperations)


type alias IntOperations int =
    { toZigZag : int -> int
    , fromZigZag : int -> int
    , toSigned : int -> int
    , fromSigned : int -> int
    , popBase128 : int -> ( Int, int )
    , pushBase128 : Int -> int -> int
    , fromBase128 : Int -> int
    }
