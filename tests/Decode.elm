module Decode exposing (suite)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Hex
import ProtoBuf.Decode as Decode
import ProtoBuf.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "Should correctly handle"
        [ describe "required fields"
            [ test "being present" <|
                \_ ->
                    Encode.message [ ( 1, Encode.string "required" ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message "" [ Decode.required 1 Decode.string updateSelf ])
                        |> Expect.equal (Just "required")
            , test "being missing" <|
                \_ ->
                    Encode.message []
                        |> Encode.encode
                        |> Decode.decode (Decode.message "" [ Decode.required 1 Decode.string updateSelf ])
                        |> Expect.equal Nothing
            , test "being invalid" <|
                \_ ->
                    Encode.message [ ( 1, Encode.int32 0 ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message "" [ Decode.required 1 Decode.string updateSelf ])
                        |> Expect.equal Nothing
            ]
        , describe "optional fields"
            [ test "being present" <|
                \_ ->
                    Encode.message [ ( 1, Encode.string "optional" ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message "default" [ Decode.optional 1 Decode.string updateSelf ])
                        |> Expect.equal (Just "optional")
            , test "being missing" <|
                \_ ->
                    Encode.message []
                        |> Encode.encode
                        |> Decode.decode (Decode.message "default" [ Decode.optional 1 Decode.string updateSelf ])
                        |> Expect.equal (Just "default")
            , test "being invalid" <|
                \_ ->
                    Encode.message [ ( 1, Encode.int32 0 ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message "default" [ Decode.optional 1 Decode.string updateSelf ])
                        |> Expect.equal Nothing
            ]
        , describe "repeated fields"
            [ test "being present" <|
                \_ ->
                    Encode.message [ ( 1, Encode.list Encode.string [ "repeated", "repeated" ] ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message [] [ Decode.repeated 1 Decode.string identity updateSelf ])
                        |> Expect.equal (Just [ "repeated", "repeated" ])
            , test "being missing" <|
                \_ ->
                    Encode.message []
                        |> Encode.encode
                        |> Decode.decode (Decode.message [] [ Decode.repeated 1 Decode.string identity updateSelf ])
                        |> Expect.equal (Just [])
            , test "being incorrect" <|
                \_ ->
                    Encode.message [ ( 1, Encode.list Encode.int32 [ 0, 0 ] ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message [] [ Decode.repeated 1 Decode.string identity updateSelf ])
                        |> Expect.equal (Just [ "\u{0000}\u{0000}" ])
            ]

        -- TODO oneOf (incl. multiple fields), map, dict, merge
        ]


updateSelf : a -> a -> a
updateSelf value _ =
    value