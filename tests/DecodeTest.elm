module DecodeTest exposing (suite)

import Expect
import Fuzz
import Int64 exposing (Int64)
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode
import Test exposing (..)
import UInt64


suite : Test
suite =
    describe "Should correctly handle"
        -- TODO oneOf (incl. multiple fields), map, dict, merge
        [ describe "required fields"
            [ test "being present" <|
                \_ ->
                    Encode.message [ ( 1, Encode.string "required" ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message "" [ Decode.required 1 Decode.string setSelf ])
                        |> Expect.equal (Just "required")
            , test "being missing" <|
                \_ ->
                    Encode.message []
                        |> Encode.encode
                        |> Decode.decode (Decode.message "" [ Decode.required 1 Decode.string setSelf ])
                        |> Expect.equal Nothing
            , test "being invalid" <|
                \_ ->
                    Encode.message [ ( 1, Encode.int32 0 ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message "" [ Decode.required 1 Decode.string setSelf ])
                        |> Expect.equal Nothing
            ]
        , describe "optional fields"
            [ test "being present" <|
                \_ ->
                    Encode.message [ ( 1, Encode.string "optional" ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message "default" [ Decode.optional 1 Decode.string setSelf ])
                        |> Expect.equal (Just "optional")
            , test "being missing" <|
                \_ ->
                    Encode.message []
                        |> Encode.encode
                        |> Decode.decode (Decode.message "default" [ Decode.optional 1 Decode.string setSelf ])
                        |> Expect.equal (Just "default")
            , test "being invalid" <|
                \_ ->
                    Encode.message [ ( 1, Encode.int32 0 ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message "default" [ Decode.optional 1 Decode.string setSelf ])
                        |> Expect.equal Nothing
            ]
        , describe "repeated fields"
            [ test "being present" <|
                \_ ->
                    Encode.message [ ( 1, Encode.list Encode.string [ "repeated", "repeated" ] ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message [] [ Decode.repeated 1 Decode.string identity setSelf ])
                        |> Expect.equal (Just [ "repeated", "repeated" ])
            , test "being missing" <|
                \_ ->
                    Encode.message []
                        |> Encode.encode
                        |> Decode.decode (Decode.message [] [ Decode.repeated 1 Decode.string identity setSelf ])
                        |> Expect.equal (Just [])
            , test "being incorrect" <|
                \_ ->
                    Encode.message [ ( 1, Encode.list Encode.int32 [ 0, 0 ] ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message [] [ Decode.repeated 1 Decode.string identity setSelf ])
                        |> Expect.equal (Just [ "\u{0000}\u{0000}" ])
            ]
        , describe "lazy decoding"
            [ test "recursion" <|
                \_ ->
                    toCommentEncoder commentValue
                        |> Encode.encode
                        |> Decode.decode commentDecoder
                        |> Expect.equal (Just commentValue)
            ]
        , describe "integers"
            [ fuzz2 Fuzz.int Fuzz.int "int64" <|
                \lower upper ->
                    let
                        i64 =
                            Int64.fromInt32s lower upper
                    in
                    Encode.message [ ( 1, Encode.int64 i64 ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message zero64 [ Decode.optional 1 Decode.int64 setSelf ])
                        |> Expect.equal (Just i64)
            , fuzz2 Fuzz.int Fuzz.int "sint64" <|
                \lower upper ->
                    let
                        i64 =
                            Int64.fromInt32s lower upper
                    in
                    Encode.message [ ( 1, Encode.sint64 i64 ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message zero64 [ Decode.optional 1 Decode.sint64 setSelf ])
                        |> Expect.equal (Just i64)
            , fuzz2 Fuzz.int Fuzz.int "uint64" <|
                \lower upper ->
                    let
                        ui64 =
                            UInt64.fromInt32s lower upper
                    in
                    Encode.message [ ( 1, Encode.uint64 ui64 ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message UInt64.zero [ Decode.optional 1 Decode.uint64 setSelf ])
                        |> Expect.equal (Just ui64)
            , fuzz2 Fuzz.int Fuzz.int "fixed64" <|
                \lower upper ->
                    let
                        f64 =
                            UInt64.fromInt32s lower upper
                    in
                    Encode.message [ ( 1, Encode.fixed64 f64 ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message UInt64.zero [ Decode.optional 1 Decode.fixed64 setSelf ])
                        |> Expect.equal (Just f64)
            , fuzz2 Fuzz.int Fuzz.int "sfixed64" <|
                \lower upper ->
                    let
                        f64 =
                            Int64.fromInt32s lower upper
                    in
                    Encode.message [ ( 1, Encode.sfixed64 f64 ) ]
                        |> Encode.encode
                        |> Decode.decode (Decode.message zero64 [ Decode.optional 1 Decode.sfixed64 setSelf ])
                        |> Expect.equal (Just f64)
            ]
        ]



-- GENERAL


setSelf : a -> a -> a
setSelf value _ =
    value



-- RECURSION


type alias Comment =
    { message : String
    , responses : Responses
    }


type Responses
    = Responses (List Comment)


commentValue : Comment
commentValue =
    Comment "It works!" (Responses [ Comment "No it doesn't." (Responses []) ])


setMessage : a -> { b | message : a } -> { b | message : a }
setMessage value model =
    { model | message = value }


setResponses : a -> { b | responses : a } -> { b | responses : a }
setResponses value model =
    { model | responses = value }


unwrapResponses : Responses -> List Comment
unwrapResponses (Responses responses) =
    responses


toCommentEncoder : Comment -> Encode.Encoder
toCommentEncoder comment =
    Encode.message
        [ ( 1, Encode.string comment.message )
        , ( 2, (Encode.list toCommentEncoder << unwrapResponses) comment.responses )
        ]


commentDecoder : Decode.Decoder Comment
commentDecoder =
    Decode.message (Comment "" (Responses []))
        [ Decode.optional 1 Decode.string setMessage
        , Decode.repeated 2
            (Decode.lazy (\_ -> commentDecoder))
            (unwrapResponses << .responses)
            (setResponses << Responses)
        ]



-- integers


zero64 : Int64
zero64 =
    Int64.fromInt32s 0 0
