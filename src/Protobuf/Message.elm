module Protobuf.Message exposing (Message, init, view, set)

{-|

@docs Message, init, view, set

-}

import Bytes
import Internal.Decode as Decode
import Internal.Encode as Encode
import Internal.Protobuf as Protobuf


{-| -}
type alias Message a =
    Protobuf.Message a


{-| -}
init : a -> Message a
init =
    Tuple.pair []


{-| -}
view : (a -> b) -> Message a -> b
view fn ( _, a ) =
    fn a


{-| -}
set : (b -> a -> a) -> b -> Message a -> Message a
set fn b ( d, a ) =
    ( d, fn b a )
