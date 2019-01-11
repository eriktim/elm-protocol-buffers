module Main exposing (Foo, jsonD, jsonE, main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Json.Decode as JD
import Json.Encode as JE
import ProtoBuf.Decode as PD
import ProtoBuf.Encode as PE


main : BenchmarkProgram
main =
    program suite


type alias Foo =
    { name : String
    , bools : List Bool
    }


jsonE : Foo -> JE.Value
jsonE foo =
    JE.object
        [ ( "name", JE.string foo.name )
        , ( "bools", JE.list JE.bool foo.bools )
        ]


jsonD : JD.Decoder Foo
jsonD =
    JD.map2 Foo
        (JD.field "name" JD.string)
        (JD.field "bools" <| JD.list JD.bool)


protoE : Foo -> PE.Encoder
protoE foo =
    PE.message
        [ ( 1, PE.string foo.name )
        , ( 2, PE.list PE.bool foo.bools )
        ]


protoD : PD.Decoder Foo
protoD =
    PD.message (Foo "" [])
        [ PD.optional 1 PD.string (\v m -> { m | name = v })
        , PD.repeated 2 PD.bool .bools (\v m -> { m | bools = v })
        ]


suite : Benchmark
suite =
    let
        sample =
            Foo "bar" (List.repeat 100 False)

        jsonString =
            JE.encode 0 (jsonE sample)

        protoBytes =
            PE.encode (protoE sample)
    in
    describe "Decode"
        [ describe "slice"
            [ benchmark "JSON" <|
                \_ -> JD.decodeString jsonD jsonString
            , benchmark "ProtoBuf" <|
                \_ -> PD.decode protoD protoBytes
            ]
        ]
