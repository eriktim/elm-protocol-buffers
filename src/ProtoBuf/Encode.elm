module ProtoBuf.Encode exposing
    ( encode, Encoder, message
    , int32, uint32, sint32, fixed32, sfixed32
    , double, float
    , string
    , bool
    , bytes
    , none
    , list, dict
    )

{-| Library for turning Elm values into
[ProtoBuf](https://developers.google.com/protocol-buffers) messages.

> The examples show `Bytes` values like this: `<3A* 05* 68 65 6C 6C 6F>`. The
> `*` means the byte is ProtoBuf _metadata_. It does not contain any real
> value. Here `3A` means the next encoded field is a _length delimited_ value
> for field number `7`. `05` is the length of the encoded value. Those five
> bytes contain the string `hello`. Read
> [this](https://developers.google.com/protocol-buffers/docs/encoding) if
> you want to learn more about how ProtoBuf encoding works.


# Encoding

@docs encode, Encoder, message


# Integers

@docs int32, uint32, sint32, fixed32, sfixed32


# Floats

@docs double, float


# Strings

@docs string


# Booleans

@docs bool


# Bytes

@docs bytes


# None

@docs none


# Data Structures

@docs list, dict

-}

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode
import Dict exposing (Dict)
import Internal.ProtoBuf exposing (WireType(..))



-- ENCODER


{-| Describes how to generate a sequence of bytes according to the
specification of ProtoBuf.
-}
type Encoder
    = Encoder WireType ( Int, Encode.Encoder )
    | ListEncoder (List Encoder)
    | NoEncoder



-- ENCODE


{-| Turn an `Encoder` into `Bytes`.

     encode (int32 127)    -- <7F>
     encode (sint32 127)   -- <FE 01>
     encode (sfixed32 127) -- <7F 00 00 00>

Values are encoded together with a field number and the
[_wire type_](https://developers.google.com/protocol-buffers/docs/encoding#structure)
conform the specification in a `.proto` file. This allows decoders to know what
field it is decoding and to read the right number of `Bytes`.

    import ProtoBuf.Encode as Encode

    type alias Person =
        { age : Int
        , name : String
        }

    encodePerson : Person -> Encode.Encoder
    encodePerson person =
        Encode.message
            [ ( 1, Encode.uint32 person.age )
            , ( 2, Encode.string person.name )
            ]

    Encode.encode (encodePerson (Person 33 "Tom")) -- <08* 21 12* 03* 54 6F 6D>

You probably want to send these `Bytes` in the body of an HTTP request:

    import Http
    import ProtoBuf.Encode as Encode

    postPerson : (Result Http.Error () -> msg) -> Person -> Cmd msg
    postPerson toMsg person =
        Http.post
            { url = "https://example.com/person"
            , body =
                Http.bytesBody "application/octet-stream" <|
                    Encode.encode (encodePerson person)
            , expect = Http.expectWhatever
            }

-}
encode : Encoder -> Bytes
encode encoder =
    case encoder of
        Encoder _ ( _, encoder_ ) ->
            Encode.encode encoder_

        ListEncoder encoders ->
            List.map (Encode.bytes << encode) encoders
                |> Encode.sequence
                |> Encode.encode

        NoEncoder ->
            Encode.encode <| Encode.sequence []


{-| Encode a record into a message. For this you need to provide a list of
**unique** field numbers (between `1` and `536870911`) and their corresponding
`Encoder`s.

     type alias Foo =
         { a : Float
         , b : String
         , c : List Int
         }

     foo : Foo
     foo =
        Foo 1.25 "hello" [ 1, 2, 3, 4, 5 ]

     encode : Encoder
     encode =
         message
             [ ( 1, double foo.a )         -- <09* 00 00 00 00 00 00 F4 3F
             , ( 2, string foo.b )         --  12* 05* 68 65 6C 6C 6F
             , ( 3, repeated int32 foo.c ) --  1A* 05* 01 02 03 04 05>
             ]

-}
message : List ( Int, Encoder ) -> Encoder
message items =
    items
        |> List.sortBy Tuple.first
        |> List.map encodeKeyValuePair
        |> sequence
        |> (\e -> Encoder (LengthDelimited (Tuple.first e)) e)



-- INTEGER


{-| Encode integers from `-2147483648` to `2147483647` into a message. Uses
variable-length encoding. Inefficient for encoding negative numbers – if your
field is likely to have negative values, use [`sint32`](#sint32) instead.

     encode (int32 0)    -- <00>
     encode (int32 100)  -- <64>
     encode (int32 -100) -- <FF FF FF FF FF FF FF 9C>

This function should also be used to encode custom types as enumeration:

    type Fruit
        = Apple
        | Banana
        | Mango
        | Unrecognized Int

    encodeFruit : Fruit -> Encoder
    encodeFruit value =
        Encode.int32 <|
            case value of
                Apple ->
                    0

                Banana ->
                    1

                Mango ->
                    2

                Unrecognized v ->
                    v

Note that for `proto2` the `Unrecognized Int` field can be left out.

-}
int32 : Int -> Encoder
int32 =
    Encoder VarInt << varInt


{-| Encode integers from `0` to `4294967295` into a message.
Uses variable-length encoding.

     encode (uint32 0)   -- <00>
     encode (uint32 100) -- <64>

-}
uint32 : Int -> Encoder
uint32 =
    Encoder VarInt << varInt << unsigned


{-| Encode integers from `-2147483648` to `2147483647` into a message. Uses
variable-length encoding. These encoder encodes negative numbers more
efficiently than [`int32`](#int32).

     encode (sint32 0)    -- <00>
     encode (sint32 100)  -- <C8 01>
     encode (sint32 -100) -- <C7 01>

-}
sint32 : Int -> Encoder
sint32 =
    Encoder VarInt << varInt << zigZag


{-| Encode integers from `0` to `4294967295` into a message. Always four bytes.
More efficient than [`uint32`](#uint32) if values are often greater than
`268435456`.

     encode (fixed32 0)   -- <00 00 00 00>
     encode (fixed32 100) -- <64 00 00 00>

-}
fixed32 : Int -> Encoder
fixed32 v =
    Encoder Bit32 ( 4, Encode.unsignedInt32 LE v )


{-| Encode integers from `-2147483648` to `2147483647` into a message.
Always four bytes.

     encode (sfixed32 0)    -- <00 00 00 00>
     encode (sfixed32 100)  -- <64 00 00 00>
     encode (sfixed32 -100) -- <9C FF FF FF>

-}
sfixed32 : Int -> Encoder
sfixed32 v =
    Encoder Bit32 ( 4, Encode.signedInt32 LE v )



-- FLOAT


{-| Encode 64-bit floating point numbers into a message.

     encode (double 0)    -- <00 00 00 00 00 00 00 00>
     encode (double 100)  -- <00 00 00 00 00 00 59 40>
     encode (double -100) -- <00 00 00 00 00 00 59 C0>

-}
double : Float -> Encoder
double v =
    Encoder Bit64 ( 8, Encode.float64 LE v )


{-| Encode 32-bit floating point numbers into a message.
The value may lose some precision by encoding it as a float.

     encode (float 0)    -- <00 00 00 00>
     encode (float 100)  -- <00 00 C8 42>
     encode (float -100) -- <00 00 C8 C2>

-}
float : Float -> Encoder
float v =
    Encoder Bit32 ( 4, Encode.float32 LE v )



-- STRING


{-| Encode strings into a message.

     encode (string "$20")   -- <24 32 30>
     encode (string "£20")   -- <C2 A3 32 30>
     encode (string "€20")   -- <E2 82 AC 32 30>
     encode (string "bread") -- <62 72 65 61 64>
     encode (string "brød")  -- <62 72 C3 B8 64>

-}
string : String -> Encoder
string v =
    let
        width =
            Encode.getStringWidth v
    in
    Encoder (LengthDelimited width) ( width, Encode.string v )



-- BOOLEAN


{-| Encode booleans into a message.

     encode (bool False) -- <00>
     encode (bool True)  -- <01>

-}
bool : Bool -> Encoder
bool v =
    Encoder VarInt <|
        if v then
            varInt 1

        else
            varInt 0



-- BYTES


{-| Copy raw `Bytes` into a message.

    -- bs == <0A 0B 0C>
    encode (bytes bs) -- <0A 0B 0C>

-}
bytes : Bytes -> Encoder
bytes v =
    let
        width =
            Bytes.width v
    in
    Encoder (LengthDelimited width) ( width, Encode.bytes v )



-- DATA STRUCTURES


{-| Encode a list of values into a message.
ProtoBuf support two kind of encodings:

     -- packed encoding
     encode
         (message
             [ ( 1, list int32 [ 1, 2, 3 ] ) -- <0A* 03* 01 02 03>
             ]
         )

     -- non-packed encoding
     encode
         (message
             [ ( 1
               , list string
                     [ "one"   -- <0A* 03* 6F 6E 65
                     , "two"   --  0A* 03* 74 77 6F
                     , "three" --  0A* 05* 74 68 72 65 65>
                     ]
               )
             ]
         )

Packed encoding is preferred as it uses less bytes on the wire. `list` will
automatically fall-back to non-packed encoding for non-scalar numeric types.

-}
list : (a -> Encoder) -> List a -> Encoder
list fn =
    ListEncoder << List.map fn


{-| Encode a dictionary of key-value pairs. This requires providing one encoder
for the keys and one for the values.

    let
        value =
            Dict.fromList
                [ ( 1, "foo" ) -- <0A* 07* 08* 01 12* 03* 66 6F 6F
                , ( 2, "bar" ) --  0A* 07* 08* 02 12* 03* 62 61 72>
                ]
    in
    message [ ( 1, dict int32 string value ) ]

-}
dict : (k -> Encoder) -> (v -> Encoder) -> Dict k v -> Encoder
dict encodeKey encodeValue dict_ =
    Dict.toList dict_
        |> List.map
            (\( key, value ) ->
                message
                    [ ( 1, encodeKey key )
                    , ( 2, encodeValue value )
                    ]
            )
        |> ListEncoder


{-| Encode nothing. Note that you can easily combine this encoder with _any_
field number to pass to [`message`](#message) as literally **nothing** will be
encoded.

This can be useful when encoding embedded messages:

    type alias Report =
        { title : String
        , contents : String
        , attachment : Maybe Attachment
        }

    encodeReport : Report -> Encoder
    encodeReport report =
        message
            [ ( 1, string report.title )
            , ( 2, string report.contents )
            , ( 3, Maybe.withDefault none <| Maybe.map encodeAttachment report.attachment )
            ]

Or when encoding custom types:

    type alias FormValue =
        { key : String
        , value : Maybe Value
        }

    type Value
        = StringValue String
        | IntValue Int

    encodeKeyValue : FormValue -> Encoder
    encodeKeyValue formValue =
        message
            [ ( 1, string formValue.key )
            , Maybe.withDefault ( 0, none ) <| Maybe.map encodeValue formValue.value
            ]

    encodeValue : Value -> ( Int, Encoder )
    encodeValue value =
        case value of
            StringValue value ->
                ( 2, string value )

            IntValue value ->
                ( 3, int32 value )

-}
none : Encoder
none =
    NoEncoder



-- BYTES ENCODER


sequence : List ( Int, Encode.Encoder ) -> ( Int, Encode.Encoder )
sequence items =
    let
        width =
            List.sum <| List.map Tuple.first items
    in
    ( width, Encode.sequence <| List.map Tuple.second items )


encodeKeyValuePair : ( Int, Encoder ) -> ( Int, Encode.Encoder )
encodeKeyValuePair ( fieldNumber, encoder ) =
    case encoder of
        Encoder wireType encoder_ ->
            sequence
                [ tag fieldNumber wireType
                , encoder_
                ]

        ListEncoder encoders ->
            case packedEncoders encoders of
                Just encoder_ ->
                    sequence
                        [ tag fieldNumber (LengthDelimited (Tuple.first encoder_))
                        , encoder_
                        ]

                Nothing ->
                    sequence <| List.map (encodeKeyValuePair << Tuple.pair fieldNumber) encoders

        NoEncoder ->
            sequence []


packedEncoders : List Encoder -> Maybe ( Int, Encode.Encoder )
packedEncoders encoders =
    case encoders of
        (Encoder wireType encoder) :: others ->
            case wireType of
                LengthDelimited _ ->
                    Nothing

                _ ->
                    Just <| sequence (encoder :: List.filterMap unwrap others)

        _ ->
            Nothing


unwrap : Encoder -> Maybe ( Int, Encode.Encoder )
unwrap encoder =
    case encoder of
        Encoder _ encoder_ ->
            Just encoder_

        _ ->
            Nothing


tag : Int -> WireType -> ( Int, Encode.Encoder )
tag fieldNumber wireType =
    let
        encodeTag base4 =
            varInt (Bitwise.or (Bitwise.shiftLeftBy 3 fieldNumber) base4)
    in
    case wireType of
        VarInt ->
            encodeTag 0

        Bit64 ->
            encodeTag 1

        LengthDelimited width ->
            sequence [ encodeTag 2, varInt width ]

        StartGroup ->
            encodeTag 3

        EndGroup ->
            encodeTag 4

        Bit32 ->
            encodeTag 5



-- VARINT


varInt : Int -> ( Int, Encode.Encoder )
varInt value =
    let
        encoders =
            varIntEncoders value
    in
    ( List.length encoders, Encode.sequence encoders )


unsigned : Int -> Int
unsigned value =
    if value >= 2 ^ 31 then
        value - 2 ^ 32

    else
        value


zigZag : Int -> Int
zigZag value =
    Bitwise.xor (Bitwise.shiftRightBy 31 value) (Bitwise.shiftLeftBy 1 value)


varIntEncoders : Int -> List Encode.Encoder
varIntEncoders value =
    let
        base128 =
            Bitwise.and 0x7F value

        higherBits =
            Bitwise.shiftRightZfBy 7 value
    in
    if higherBits /= 0x00 then
        Encode.unsignedInt8 (Bitwise.or 0x80 base128) :: varIntEncoders higherBits

    else
        [ Encode.unsignedInt8 base128 ]
