module Util exposing (bytes, dict, expectMessage, fieldNumber, float32, int32, uint32)

import Bytes
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, float, int, list, maybe, string)
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode



-- FUZZER


fieldNumber : Fuzzer Int
fieldNumber =
    Fuzz.intRange 1 (2 ^ 29 - 1)


int32 : Fuzzer Int
int32 =
    Fuzz.intRange (-2 ^ 31) (2 ^ 31 - 1)


uint32 : Fuzzer Int
uint32 =
    -- max `Fuzz.int` is only `2 ^ 31 - 1`
    Fuzz.floatRange 0 (2 ^ 32 - 1)
        |> Fuzz.map round


float32 : Fuzzer Float
float32 =
    Fuzz.map
        (\v ->
            Bytes.Encode.encode (Bytes.Encode.float32 Bytes.LE v)
                |> Bytes.Decode.decode (Bytes.Decode.float32 Bytes.LE)
                |> Maybe.withDefault v
        )
        float


bytes : Fuzzer Bytes.Bytes
bytes =
    Fuzz.map (Bytes.Encode.encode << Bytes.Encode.sequence << List.map (Bytes.Encode.signedInt32 Bytes.LE)) (list int32)


dict : Fuzzer comparable -> Fuzzer v -> Fuzzer (Dict comparable v)
dict k v =
    Fuzz.map2 (\ks vs -> Dict.fromList <| List.map2 Tuple.pair ks vs) (list k) (list v)



-- EXPECT


expectMessage : (a -> Encode.Encoder) -> Decode.Decoder a -> a -> Expectation
expectMessage encoder decoder message =
    Encode.encode (encoder message)
        |> Decode.decode decoder
        |> Expect.equal (Just message)
