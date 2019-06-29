# Protocol Buffers [![Build Status](https://travis-ci.org/eriktim/elm-protocol-buffers.svg?branch=master)](https://travis-ci.org/eriktim/elm-protocol-buffers)

This package lets you create encoders and decoders for (de)serializing data
according to the [Protobuf](https://developers.google.com/protocol-buffers)
specification. Typically, **you should not use this package directly** as the
required encoders and decoders can be generated directly from `.proto`
specification files, e.g. by using this
[`protoc` plugin](https://www.npmjs.com/package/protoc-gen-elm).

> Protocol buffers are Google's language-neutral, platform-neutral, extensible
> mechanism for serializing structured data – think XML, but smaller, faster,
> and simpler. You define how you want your data to be structured once, then
> you can use special generated source code to easily write and read your
> structured data to and from a variety of data streams and using a variety of
> languages.

## Motivation

As Evan explained in his vision on
[data interchange](https://gist.github.com/evancz/1c5f2cf34939336ecb79b97bb89d9da6),
there are many ways to send information between clients and servers, like JSON,
XML, GraphQL and the one this package provides: **Protocol Buffers**.

Where Elm spoils us with an excellent type system, we lose this safety at the
boundaries of our application, e.g. when talking to servers. Protobuf forces
you to specify the interchange format explicity up front, making it
**type-safe**. It also:

* makes you **decouple** code for data exchange from your business logic,
  enabling your application to evolve over time;
* makes you send **fewer bytes** over the wire;
* allows you to **generate code** using the same data interchange format for
  your backend as well;
* is Google's **_lingua franca_** for data.

This package provides an API to help converting between Elm values and bytes
by implementing Protocol Buffers. `elm-protocol-buffers` also opens the door to
add support for the communication protocol
[gRPC](https://grpc.io/docs/guides/index.html), which could be an interesting
next step.

## Extensive Example

Given a Protobuf `.proto` file

```protobuf
message Person {
  required string name = 1;
  required int32 id = 2;
  optional string email = 3;

  enum PhoneType {
    MOBILE = 0;
    HOME = 1;
    WORK = 2;
  }

  message PhoneNumber {
    required string number = 1;
    optional PhoneType type = 2 [default = HOME];
  }

  repeated PhoneNumber phone = 4;
}
```

this package handles converting between `Person` and `Bytes` values:

```elm
import Protobuf.Decode as Decode
import Protobuf.Encode as Encode



-- MODEL


type alias Person =
    { name : String
    , id : Int
    , email : String
    , phone : List PhoneNumber
    }


type alias PhoneNumber =
    { number : String
    , type_ : PhoneType
    }


type PhoneType
    = Mobile
    | Home
    | Work



-- ENCODE


toPersonEncoder : Person -> Encode.Encoder
toPersonEncoder person =
    Encode.message
        [ ( 1, Encode.string person.name )
        , ( 2, Encode.int32 person.id )
        , ( 3, Encode.string person.email )
        , ( 4, Encode.list toPhoneNumberEncoder person.phone )
        ]


toPhoneNumberEncoder : PhoneNumber -> Encode.Encoder
toPhoneNumberEncoder phoneNumber =
    Encode.message
        [ ( 1, Encode.string phoneNumber.number )
        , ( 2, toPhoneTypeEncoder phoneNumber.type_ )
        ]


toPhoneTypeEncoder : PhoneType -> Encode.Encoder
toPhoneTypeEncoder phoneType =
    case phoneType of
        Mobile ->
            Encode.int32 0

        Home ->
            Encode.int32 1

        Work ->
            Encode.int32 2



-- DECODE


personDecoder : Decode.Decoder Person
personDecoder =
    Decode.message (Person "" 0 "" [])
        [ Decode.required 1 Decode.string setName
        , Decode.required 2 Decode.int32 setId
        , Decode.optional 3 Decode.string setEmail
        , Decode.repeated 4 phoneNumberDecoder .phone setPhone
        ]


phoneNumberDecoder : Decode.Decoder PhoneNumber
phoneNumberDecoder =
    Decode.message (PhoneNumber "" Home)
        [ Decode.required 1 Decode.string setNumber
        , Decode.optional 2 phoneTypeDecoder setType
        ]


phoneTypeDecoder : Decode.Decoder PhoneType
phoneTypeDecoder =
    Decode.int32
        |> Decode.map
            (\value ->
                case value of
                    0 ->
                        Mobile

                    1 ->
                        Home

                    2 ->
                        Work

                    _ ->
                        Home
            )



-- SETTERS


setName : a -> { b | name : a } -> { b | name : a }
setName value model =
    { model | name = value }


setId : a -> { b | id : a } -> { b | id : a }
setId value model =
    { model | id = value }


setEmail : a -> { b | email : a } -> { b | email : a }
setEmail value model =
    { model | email = value }


setPhone : a -> { b | phone : a } -> { b | phone : a }
setPhone value model =
    { model | phone = value }


setNumber : a -> { b | number : a } -> { b | number : a }
setNumber value model =
    { model | number = value }


setType : a -> { b | type_ : a } -> { b | type_ : a }
setType value model =
    { model | type_ = value }
```

## Known Limitations

This packages aims to support both `proto2` and `proto3`. However, there are
some limitations:

* JavaScript uses 64-bit floating point
  [numbers](https://tc39.github.io/ecma262/#sec-ecmascript-language-types-number-type)
  for both integers and floats. Integers are only considered safe to use up to
  about 54 bits. Therefore **all 64-bit integer variants are not supported** by
  this package.

* Unknown fields are well-formed protocol buffer serialized data representing
  fields that the decoder does not recognize (for example when the field is
  removed from the `.proto` file but the server is not yet updated). Currently,
  these fields are ignored and hence are **lost when re-serializing messages**.

* [extensions](https://developers.google.com/protocol-buffers/docs/proto#extensions)
  are currently **not supported**.

