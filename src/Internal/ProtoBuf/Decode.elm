module Internal.ProtoBuf.Decode exposing
    ( FieldData(..)
    , Message
    , allChunks
    , int32Decoder
    , lastChunk
    , lastField
    , messageDecoder
    )

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict exposing (Dict)
import Internal.ProtoBuf exposing (WireType(..))


type Message
    = Message (Dict Int Field)


type alias Field =
    { wireType : WireType
    , index : Int
    , chunks : List Bytes
    }


type FieldData a
    = FieldData a
    | NotPresent
    | Invalid


lastChunk : Int -> WireType -> Message -> FieldData Bytes
lastChunk fieldNumber wireType (Message dict) =
    case Dict.get fieldNumber dict of
        Just field ->
            if field.wireType == wireType then
                List.head field.chunks
                    |> Maybe.map FieldData
                    |> Maybe.withDefault NotPresent

            else
                Invalid

        Nothing ->
            NotPresent


allChunks : Int -> WireType -> Message -> FieldData (List Bytes)
allChunks fieldNumber wireType (Message dict) =
    case Dict.get fieldNumber dict of
        Just field ->
            if field.wireType == wireType then
                FieldData field.chunks

            else
                Invalid

        Nothing ->
            NotPresent


lastField : List ( Int, WireType ) -> Message -> FieldData ( Int, Bytes )
lastField fields (Message dict) =
    let
        results =
            fields
                |> List.map
                    (\( fieldNumber, wireType ) ->
                        case Dict.get fieldNumber dict of
                            Just field ->
                                if field.wireType == wireType then
                                    List.head field.chunks
                                        |> Maybe.map (\bs -> FieldData ( fieldNumber, bs ))
                                        |> Maybe.withDefault NotPresent
                                        |> Tuple.pair field.index

                                else
                                    ( 0, Invalid )

                            Nothing ->
                                ( 0, NotPresent )
                    )
                |> List.filter ((/=) NotPresent << Tuple.second)
    in
    if List.member Invalid (List.map Tuple.second results) then
        Invalid

    else
        results
            |> List.sortBy Tuple.first
            |> List.reverse
            |> List.head
            |> Maybe.map Tuple.second
            |> Maybe.withDefault NotPresent



-- DECODERS


messageDecoder : Int -> Decode.Decoder Message
messageDecoder width =
    Decode.loop ( width, 0, Dict.empty ) messageStepDecoder


messageStepDecoder : ( Int, Int, Dict Int Field ) -> Decode.Decoder (Decode.Step ( Int, Int, Dict Int Field ) Message)
messageStepDecoder ( bytesRemaining, index, dict ) =
    if bytesRemaining <= 0 then
        Decode.succeed (Decode.Done (Message dict))

    else
        messageFieldDecoder
            |> Decode.map
                (\{ fieldNumber, usedBytes, wireType, bs } ->
                    let
                        newDict =
                            Dict.update fieldNumber
                                (\field ->
                                    Just
                                        { wireType = wireType
                                        , index = index
                                        , chunks = bs :: Maybe.withDefault [] (Maybe.map .chunks field)
                                        }
                                )
                                dict

                        newBytesRemaining =
                            bytesRemaining - Bytes.width bs - usedBytes
                    in
                    Decode.Loop ( newBytesRemaining, index + 1, newDict )
                )


messageFieldDecoder : Decode.Decoder { fieldNumber : Int, usedBytes : Int, wireType : WireType, bs : Bytes }
messageFieldDecoder =
    tagDecoder
        |> Decode.andThen
            (\( usedBytes, fieldNumber, wireType ) ->
                case wireType of
                    VarInt ->
                        varIntListDecoder
                            |> Decode.map
                                (\values ->
                                    let
                                        bs =
                                            Encode.encode <| Encode.sequence (List.map Encode.unsignedInt8 values)
                                    in
                                    { fieldNumber = fieldNumber
                                    , usedBytes = usedBytes
                                    , wireType = wireType
                                    , bs = bs
                                    }
                                )

                    Bit64 ->
                        Decode.bytes 8
                            |> Decode.map
                                (\bs ->
                                    { fieldNumber = fieldNumber
                                    , usedBytes = usedBytes
                                    , wireType = wireType
                                    , bs = bs
                                    }
                                )

                    LengthDelimited ->
                        int32Decoder
                            |> Decode.andThen
                                (\( n, length ) ->
                                    Decode.bytes length
                                        |> Decode.map
                                            (\bs ->
                                                { fieldNumber = fieldNumber
                                                , usedBytes = n + usedBytes
                                                , wireType = wireType
                                                , bs = bs
                                                }
                                            )
                                )

                    StartGroup ->
                        Decode.fail

                    EndGroup ->
                        Decode.fail

                    Bit32 ->
                        Decode.bytes 4
                            |> Decode.map
                                (\bs ->
                                    { fieldNumber = fieldNumber
                                    , usedBytes = usedBytes
                                    , wireType = wireType
                                    , bs = bs
                                    }
                                )
            )


tagDecoder : Decode.Decoder ( Int, Int, WireType )
tagDecoder =
    int32Decoder
        |> Decode.andThen
            (\( usedBytes, value ) ->
                let
                    wireType =
                        case Bitwise.and 0x07 value of
                            0 ->
                                Just VarInt

                            1 ->
                                Just Bit64

                            2 ->
                                Just LengthDelimited

                            3 ->
                                Just StartGroup

                            4 ->
                                Just EndGroup

                            5 ->
                                Just Bit32

                            _ ->
                                Nothing
                in
                case wireType of
                    Just wireType_ ->
                        Decode.succeed ( usedBytes, Bitwise.shiftRightZfBy 3 value, wireType_ )

                    Nothing ->
                        Decode.fail
            )


varIntListDecoder : Decode.Decoder (List Int)
varIntListDecoder =
    Decode.unsignedInt8
        |> Decode.andThen
            (\value ->
                if Bitwise.and 0x80 value == 0x80 then
                    Decode.map (\values -> value :: values) varIntListDecoder

                else
                    Decode.succeed [ value ]
            )


int32Decoder : Decode.Decoder ( Int, Int )
int32Decoder =
    varIntListDecoder
        |> Decode.map
            (\values ->
                let
                    usedBytes =
                        List.length values

                    values_ =
                        List.map (Bitwise.and 0x7F) values

                    offsets =
                        List.map ((*) 7) <|
                            List.range 0 (List.length values)
                in
                ( usedBytes, List.sum <| List.map2 Bitwise.shiftLeftBy offsets values_ )
            )
