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
