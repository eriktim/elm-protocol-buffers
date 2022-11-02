module Protobuf.Decode exposing
    ( Decoder, decode, expectBytes, FieldDecoder, message
    , required, optional, repeated, mapped, oneOf
    , int32, uint32, sint32, fixed32, sfixed32, int64, uint64, sint64, fixed64, sfixed64
    , double, float
    , string
    , bool
    , bytes
    , map
    , lazy
    )

{-| Library for turning
[Protobuf](https://developers.google.com/protocol-buffers) messages into Elm
values.


# Decoding

@docs Decoder, decode, expectBytes, FieldDecoder, message


# Field Decoders

@docs required, optional, repeated, mapped, oneOf


# Integers

@docs int32, uint32, sint32, fixed32, sfixed32, int64, uint64, sint64, fixed64, sfixed64


# Floats

@docs double, float


# Strings

@docs string


# Booleans

@docs bool


# Bytes

@docs bytes


# Map

@docs map


# Lazy

@docs lazy

-}

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import Dict exposing (Dict)
import Http
import Internal.Int32
import Internal.Int64
import Internal.IntOperations exposing (IntOperations)
import Internal.Protobuf exposing (WireType(..))
import Protobuf.Types.Int64 as Int64 exposing (Int64)
import Set



-- DECODER


{-| Describes how to turn a sequence of Protobuf-encoded bytes into a nice Elm value.

    import Protobuf.Decode as Decode

    type alias Person =
        { age : Int
        , name : String
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        Decode.message (Person 0 "")
            |> Decode.optional 1 Decode.int32 setAge
            |> Decode.optional 2 Decode.string setName

    -- SETTERS
    setAge : a -> { b | age : a } -> { b | age : a }
    setAge value model =
        { model | age = value }

    setName : a -> { b | name : a } -> { b | name : a }
    setName value model =
        { model | name = value }

-}
type Decoder a
    = Decoder (WireType -> Decode.Decoder ( Int, a ))


{-| Describes how to decode a certain field in a Protobuf-encoded message and
how to update a record with the new Elm value.
-}
type FieldDecoder a
    = FieldDecoder Bool (List ( Int, Decoder (a -> a) ))



-- DECODE


{-| Turn a sequence of bytes into a nice Elm value.

     decode int32 <7F>    -- Just 127
     decode sint32 <7F>   -- Just -64
     decode sfixed32 <7F> -- Nothing

The `Decoder` specifies exactly how this should happen. This process may fail
if:

  - a required field is not present (`proto2` only);
  - there is a mismatch of the
    [_wire type_](https://developers.google.com/protocol-buffers/docs/encoding#structure)
    of the encoded value and the decoder;
  - the sequence of bytes is corrupted or unexpected somehow.

The examples above show a case where there are not enough bytes. They also show
the same bytes sequence can lead to different values depending on the `Decoder`
that is being used. Decoders cannot always detect these kind of mismatches.

Values are always encoded together with a field number and their
[_wire type_](https://developers.google.com/protocol-buffers/docs/encoding#structure)
. This allows the decoder to set the right fields and to process the correct
number of bytes.

-}
decode : Decoder a -> Bytes -> Maybe a
decode (Decoder decoder) bs =
    let
        wireType =
            LengthDelimited (Bytes.width bs)
    in
    Decode.decode (decoder wireType) bs
        |> Maybe.map Tuple.second


{-| Turn a [`Decoder`](#Decoder) into a `Http.Expect`. You probably received
the `Bytes` you want to decode from an HTTP request. As [`message`](#message)
consumes **all remaining bytes** on the wire, you cannot use `Http.expectBytes`
directly (as it is not aware of the width of the bytes sequence). Hence, you
might want to use the `expectBytes` as provided by this package.

    import Http
    import Protobuf.Decode as Decode

    getPerson : (Result Http.Error a -> msg) -> Cmd msg
    getPerson toMsg =
        Http.get
          { url = "https://example.com/person"
          , Decode.expectBytes toMsg personDecoder
          }

-}
expectBytes : (Result Http.Error a -> msg) -> Decoder a -> Http.Expect msg
expectBytes toMsg decoder =
    Http.expectBytesResponse toMsg
        (\response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case decode decoder body of
                        Just value ->
                            Ok value

                        Nothing ->
                            Err (Http.BadBody "Protobuf decoder error")
        )


{-| Decode **all remaining bytes** into an record. The initial value given here
holds all default values (which cannot be overridden for `proto3`). Each
provided field decoder calls a setter function to update the record when its
field number is encountered on the bytes sequence. _Unknown fields_ that have
no matching field decoder are currently being ignored.

    import Protobuf.Decode as Decode

    type alias Person =
        { name : String
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        -- Person "John"
        Decode.message (Person "John") []

-}
message : a -> List (FieldDecoder a) -> Decoder a
message v fieldDecoders =
    let
        ( requiredSet, dict ) =
            fieldDecoders
                |> List.foldr
                    (\(FieldDecoder isRequired items) ( numbers, decoders ) ->
                        let
                            numbers_ =
                                if isRequired then
                                    numbers ++ List.map Tuple.first items

                                else
                                    numbers
                        in
                        ( numbers_, items ++ decoders )
                    )
                    ( [], [] )
                |> Tuple.mapFirst Set.fromList
                |> Tuple.mapSecond Dict.fromList
    in
    Decoder
        (\wireType ->
            case wireType of
                LengthDelimited width ->
                    Decode.loop
                        { width = width
                        , requiredFieldNumbers = requiredSet
                        , dict = dict
                        , model = v
                        }
                        (stepMessage width)

                _ ->
                    Decode.fail
        )



-- FIELD DECODERS


{-| Decode a required field. Decoding a message fails when one of its required
fields is not present in the bytes sequence. Required fields are only supported
in `proto2`.

    type alias Person =
        { age : Int -- field number 1
        , name : String -- field number 3
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        -- <08 21 1A 04 4A 6F 68 6E> == Just (Person 33 "John")
        -- <08 21>                   == Nothing
        -- <>                        == Nothing
        Decode.message (Person 0 "")
            [ Decode.required 1 int32 setAge
            , Decode.required 3 string setName
            ]

    -- SETTERS
    setAge : a -> { b | age : a } -> { b | age : a }
    setAge value model =
        { model | age = value }

    setName : a -> { b | name : a } -> { b | name : a }
    setName value model =
        { model | name = value }

-}
required : Int -> Decoder a -> (a -> b -> b) -> FieldDecoder b
required fieldNumber decoder set =
    FieldDecoder True [ ( fieldNumber, map set decoder ) ]


{-| Decode an optional field.

    import Protobuf.Decode as Decode

    type alias Person =
        { age : Int -- field number 2
        , name : String -- field number 4
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        -- <08 21 1A 04 4A 6F 68 6E> == Just (Person 33 "John")
        -- <08 21>                   == Just (Person 33 "")
        -- <>                        == Just (Person 0 "")
        Decode.message (Person 0 "")
            [ Decode.optional 2 int32 setAge
            , Decode.optional 4 string setName
            ]

    -- SETTERS
    setAge : a -> { b | age : a } -> { b | age : a }
    setAge value model =
        { model | age = value }

    setName : a -> { b | name : a } -> { b | name : a }
    setName value model =
        { model | name = value }

-}
optional : Int -> Decoder a -> (a -> b -> b) -> FieldDecoder b
optional fieldNumber decoder set =
    FieldDecoder False [ ( fieldNumber, map set decoder ) ]


{-| Decode a repeated field. If no such fields are present when decoding a
message, the result will be an empty list.

As repeated fields may occur multiple times in a bytes sequence, `repeated`
also needs to get hold of the record's current value in order to append the new
value.

    import Protobuf.Decode as Decode

    type alias Person =
        { names : List String -- field number 5
        }

    personDecoder : Decode.Decoder Person
    personDecoder =
        -- <2A 04 4A 6F 68 6E 2A 07 4D 61 72 77 6F 6F 64> == Just (Person [ "John", "Marwood" ])
        -- <2A 04 4A 6F 68 6E>                            == Just (Person [ "John" ])
        -- <>                                             == Just (Person [])
        Decode.message (Person [])
            [ Decode.repeated 5 string .names setNames
            ]

    -- SETTERS
    setNames : a -> { b | names : a } -> { b | names : a }
    setNames value model =
        { model | names = value }

-}
repeated : Int -> Decoder a -> (b -> List a) -> (List a -> b -> b) -> FieldDecoder b
repeated fieldNumber (Decoder decoder) get set =
    let
        listDecoder =
            Decoder
                (\wireType ->
                    case wireType of
                        LengthDelimited width ->
                            Decode.loop ( width, [] ) (stepPackedField width (decoder wireType))

                        _ ->
                            Decode.map (Tuple.mapSecond List.singleton) (decoder wireType)
                )

        update value model =
            set (get model ++ value) model
    in
    FieldDecoder False [ ( fieldNumber, map update listDecoder ) ]


{-| Decode a map field. If no such fields are present when decoding a message,
the result will be an empty `Dict`. Note that you need to provide one decoder
for the keys and another one for the values. Keys without a value or values
without a key stick to the provided defaults.

As map fields may occur multiple times in a bytes sequence, `mapped`
also needs to get hold of the record's current value in order to append the new
value.

    import Dict exposing (Dict)
    import Protobuf.Decode as Decode

    type alias Administration =
        { persons : Dict Int String -- field number 6
        }

    administrationDecoder : Decode.Decoder Administration
    administrationDecoder =
        -- <32 08 08 01 12 04 4A 6F 68 6E 32 08 08 02 12 04 4B 61 74 65> == Just (Administration (Dict.fromList [( 1, "John" ), ( 2, "Kate" )])
        -- <32 08 08 01 12 04 4A 6F 68 6E>                               == Just (Administration (Dict.fromList [( 1, "John" )])
        -- <32 08 08 01>                                                 == Just (Administration (Dict.fromList [( 1, "" )])
        -- <>                                                            == Just (Administration Dict.empty)
        Decode.message (Administration Dict.empty)
            [ Decode.mapped 6 ( 0, "" ) int32 string .persons setPersons
            ]

    -- SETTERS
    setPersons : a -> { b | persons : a } -> { b | persons : a }
    setPersons value model =
        { model | persons = value }

-}
mapped : Int -> ( comparable, a ) -> Decoder comparable -> Decoder a -> (b -> Dict comparable a) -> (Dict comparable a -> b -> b) -> FieldDecoder b
mapped fieldNumber defaultTuple keyDecoder valueDecoder get set =
    let
        decoder =
            message defaultTuple
                [ optional 1 keyDecoder (\key ( _, value ) -> ( key, value ))
                , optional 2 valueDecoder (\value ( key, _ ) -> ( key, value ))
                ]
    in
    repeated fieldNumber decoder (Dict.toList << get) (set << Dict.fromList)


{-| Decode one of some fields. As the decoder is capable of deserializing
different types of data its return type must be a custom type.

    import Protobuf.Decode as Decode

    type alias FormValue =
        { key : String -- field number 7
        , value : Maybe Value -- field number 8 or 9
        }

    type Value
        = StringValue String
        | IntValue Int

    formValueDecoder : Decode.Decoder FormValue
    formValueDecoder =
        -- <0A 03 6B 65 79 12 05 76 61 6C 75 65> == Just (FormValue "key" (StringValue "value"))
        -- <0A 03 6B 65 79 10 64>                == Just (FormValue "key" (IntValue 100))
        -- <0A 03 6B 65 79>                      == Just (FormValue "key" NoValue)
        -- <>                                    == Just (FormValue "" NoValue)
        Decode.message (FormValue "" NoValue)
            [ Decode.optional 7 string setKey
            , Decode.oneOf
                [ ( 8, Decode.map StringValue Decode.string )
                , ( 9, Decode.map IntValue Decode.int32 )
                ]
                setValue
            ]

    -- SETTERS
    setKey : a -> { b | key : a } -> { b | key : a }
    setKey value model =
        { model | key = value }

    setValue : a -> { b | value : a } -> { b | value : a }
    setValue value model =
        { model | value = value }

-}
oneOf : List ( Int, Decoder a ) -> (Maybe a -> b -> b) -> FieldDecoder b
oneOf decoders set =
    decoders
        |> List.map (Tuple.mapSecond (map (set << Just)))
        |> FieldDecoder False



-- INTEGER


{-| Decode a variable number of bytes into an integer from -2147483648 to 2147483647.
-}
int32 : Decoder Int
int32 =
    intDecoder Internal.Int32.operations


{-| Decode a variable number of bytes into an integer from 0 to 4294967295.
-}
uint32 : Decoder Int
uint32 =
    uintDecoder Internal.Int32.operations


{-| Decode a variable number of bytes into an integer from -2147483648 to 2147483647.
-}
sint32 : Decoder Int
sint32 =
    sintDecoder Internal.Int32.operations


{-| Decode four bytes into an integer from 0 to 4294967295.
-}
fixed32 : Decoder Int
fixed32 =
    packedDecoder Bit32 (Decode.map (Tuple.pair 4) (Decode.unsignedInt32 LE))


{-| Decode four bytes into an integer from -2147483648 to 2147483647.
-}
sfixed32 : Decoder Int
sfixed32 =
    packedDecoder Bit32 (Decode.map (Tuple.pair 4) (Decode.signedInt32 LE))


{-| Decode a variable number of bytes into an integer from `-9223372036854775808` to `9223372036854775807`.
-}
int64 : Decoder Int64
int64 =
    intDecoder Internal.Int64.operations


{-| Decode a variable number of bytes into an integer from `-9223372036854775808` to `9223372036854775807`.
-}
sint64 : Decoder Int64
sint64 =
    sintDecoder Internal.Int64.operations


{-| Decode a variable number of bytes into an integer from `0` to `18446744073709551615`.
-}
uint64 : Decoder Int64
uint64 =
    uintDecoder Internal.Int64.operations


{-| Decode a eight bytes into an integer from `0` to `18446744073709551615`.
-}
fixed64 : Decoder Int64
fixed64 =
    fixed64Decoder <|
        Decode.map2 (\lower higher -> Int64.fromInts higher lower)
            (Decode.unsignedInt32 LE)
            (Decode.unsignedInt32 LE)


{-| Decode eight bytes into an integer from `-9223372036854775808` to `9223372036854775807`.
-}
sfixed64 : Decoder Int64
sfixed64 =
    fixed64


{-| Decode 64 bits into a data type of your choice.
If you are using this function make sure that you are always consuming exactly 64 bits.
-}
fixed64Decoder : Decode.Decoder int64Type -> Decoder int64Type
fixed64Decoder decoder =
    decoder
        |> Decode.map (Tuple.pair 8)
        |> packedDecoder Bit64



-- FLOAT


{-| Decode eight bytes into a floating point number.
-}
double : Decoder Float
double =
    packedDecoder Bit64 (Decode.map (Tuple.pair 8) (Decode.float64 LE))


{-| Decode four bytes into a floating point number.
-}
float : Decoder Float
float =
    packedDecoder Bit32 (Decode.map (Tuple.pair 4) (Decode.float32 LE))



-- STRING


{-| Decode all bytes into a string.
-}
string : Decoder String
string =
    lengthDelimitedDecoder Decode.string



-- BOOLEAN


{-| Decode one byte into a boolean.
-}
bool : Decoder Bool
bool =
    packWith ((/=) 0) Internal.Int32.operations
        |> packedDecoder VarInt



-- BYTES


{-| Copy all bytes into a new `Bytes` sequence.
-}
bytes : Decoder Bytes
bytes =
    lengthDelimitedDecoder Decode.bytes



-- MAP


{-| Transform the value produced by a decoder.
This is useful when encoding custom types as an enumeration:

    type Fruit
        = Apple
        | Banana
        | Mango
        | Unrecognized Int

    fruitDecoder : Decoder Fruit
    fruitDecoder =
        Decode.int32
            |> Decode.map
                (\value ->
                    case value of
                        0 ->
                            Apple

                        1 ->
                            Banana

                        2 ->
                            Mango

                        v ->
                            Unrecognized v
                )

`Unrecognized Int` is only used for values that are present but not known. For
`proto2` decoding it is left out and unrecognized values are left out.

-}
map : (a -> b) -> Decoder a -> Decoder b
map fn (Decoder decoder) =
    Decoder (\wireType -> Decode.map (Tuple.mapSecond fn) (decoder wireType))



-- LAZY


{-| Sometimes you have messages with a recursive structure, like nested
comments. You must use `lazy`to make sure your decoder unrolls lazily.

    type alias Comment =
        { message : String
        , responses : Responses
        }

    type Responses
        = Responses (List Comment)

    commentDecoder : Decoder Comment
    commentDecoder =
        Decode.message (Comment "" (Responses []))
            [ Decode.optional 1 Decode.string setMessage
            , Decode.repeated 2
                (Decode.lazy (\_ -> commentDecoder))
                (unwrapResponses << .responses)
                (setResponses << Responses)
            ]

    -- SETTERS
    setMessage : a -> { b | message : a } -> { b | message : a }
    setMessage value model =
        { model | message = value }

    setResponses : a -> { b | responses : a } -> { b | responses : a }
    setResponses value model =
        { model | responses = value }

    unwrapResponses : Responses -> List Comment
    unwrapResponses (Responses responses) =
        responses

[Here](https://elm-lang.org/0.19.0/bad-recursion) you can read more about
recursive data structures.

-}
lazy : (() -> Decoder a) -> Decoder a
lazy delayedDecoder =
    Decoder
        (\wireType ->
            Decode.succeed ()
                |> Decode.andThen
                    (\v ->
                        let
                            (Decoder decoder) =
                                delayedDecoder v
                        in
                        decoder wireType
                    )
        )



-- BYTES DECODER


type alias DecodeState a =
    { width : Int
    , requiredFieldNumbers : Set.Set Int
    , dict : Dict.Dict Int (Decoder (a -> a))
    , model : a
    }


stepMessage : Int -> DecodeState a -> Decode.Decoder (Decode.Step (DecodeState a) ( Int, a ))
stepMessage width state =
    if state.width <= 0 then
        if Set.isEmpty state.requiredFieldNumbers then
            Decode.succeed (Decode.Done ( width, state.model ))

        else
            Decode.fail

    else
        tagDecoder
            |> Decode.andThen
                (\( usedBytes, ( fieldNumber, wireType ) ) ->
                    case Dict.get fieldNumber state.dict of
                        Just (Decoder decoder) ->
                            Decode.map
                                (\( n, fn ) ->
                                    Decode.Loop
                                        { state
                                            | width = state.width - usedBytes - n
                                            , requiredFieldNumbers = Set.remove fieldNumber state.requiredFieldNumbers
                                            , model = fn state.model
                                        }
                                )
                                (decoder wireType)

                        Nothing ->
                            unknownFieldDecoder wireType
                                |> Decode.map (\n -> Decode.Loop { state | width = state.width - usedBytes - n })
                )


tagDecoder : Decode.Decoder ( Int, ( Int, WireType ) )
tagDecoder =
    pack Internal.Int32.operations
        |> Decode.andThen
            (\( usedBytes, value ) ->
                let
                    fieldNumber =
                        Bitwise.shiftRightZfBy 3 value
                in
                Decode.map (\( n, wireType ) -> ( usedBytes + n, ( fieldNumber, wireType ) )) <|
                    case Bitwise.and 0x07 value of
                        0 ->
                            Decode.succeed ( 0, VarInt )

                        1 ->
                            Decode.succeed ( 0, Bit64 )

                        2 ->
                            Decode.map (Tuple.mapSecond LengthDelimited) (pack Internal.Int32.operations)

                        3 ->
                            Decode.succeed ( 0, StartGroup )

                        4 ->
                            Decode.succeed ( 0, EndGroup )

                        5 ->
                            Decode.succeed ( 0, Bit32 )

                        _ ->
                            Decode.fail
            )


pack : IntOperations int -> Decode.Decoder ( Int, int )
pack =
    packWith identity


packWith : (int -> a) -> IntOperations int -> Decode.Decoder ( Int, a )
packWith transform config =
    varIntDecoder config
        |> Decode.map (Tuple.mapSecond transform)


intDecoder : IntOperations int -> Decoder int
intDecoder config =
    pack config |> packedDecoder VarInt


sintDecoder : IntOperations int -> Decoder int
sintDecoder config =
    packWith config.zagZig config |> packedDecoder VarInt


uintDecoder : IntOperations int -> Decoder int
uintDecoder config =
    packWith config.toUnsigned config |> packedDecoder VarInt


varIntDecoder : IntOperations int -> Decode.Decoder ( Int, int )
varIntDecoder config =
    Decode.unsignedInt8
        |> Decode.andThen
            (\octet ->
                if Bitwise.and 0x80 octet == 0x80 then
                    Decode.map
                        (\( usedBytes, value ) ->
                            ( usedBytes + 1, config.pushBase128 (Bitwise.and 0x7F octet) value )
                        )
                        (varIntDecoder config)

                else
                    Decode.succeed ( 1, config.fromBase128 octet )
            )


lengthDelimitedDecoder : (Int -> Decode.Decoder a) -> Decoder a
lengthDelimitedDecoder decoder =
    Decoder
        (\wireType ->
            case wireType of
                LengthDelimited width ->
                    Decode.map (Tuple.pair width) (decoder width)

                _ ->
                    Decode.fail
        )


packedDecoder : WireType -> Decode.Decoder ( Int, a ) -> Decoder a
packedDecoder decoderWireType decoder =
    Decoder
        (\wireType ->
            case wireType of
                LengthDelimited _ ->
                    decoder

                _ ->
                    if wireType == decoderWireType then
                        decoder

                    else
                        Decode.fail
        )


stepPackedField : Int -> Decode.Decoder ( Int, a ) -> ( Int, List a ) -> Decode.Decoder (Decode.Step ( Int, List a ) ( Int, List a ))
stepPackedField fullWidth decoder ( width, values ) =
    Decode.map
        (\( w, value ) ->
            let
                bytesRemaining =
                    width - w

                values_ =
                    value :: values
            in
            if bytesRemaining <= 0 then
                Decode.Done ( fullWidth, List.reverse values_ )

            else
                Decode.Loop ( bytesRemaining, values_ )
        )
        decoder


unknownFieldDecoder : WireType -> Decode.Decoder Int
unknownFieldDecoder wireType =
    case wireType of
        VarInt ->
            Decode.map Tuple.first (varIntDecoder Internal.Int32.operations)

        Bit64 ->
            Decode.map (always 8) (Decode.bytes 8)

        LengthDelimited width ->
            Decode.map (always width) (Decode.bytes width)

        StartGroup ->
            Decode.fail

        EndGroup ->
            Decode.fail

        Bit32 ->
            Decode.map (always 4) (Decode.bytes 4)
