# Protocol Buffers [![Build Status](https://travis-ci.org/eriktim/elm-protocol-buffers.svg?branch=master)](https://travis-ci.org/eriktim/elm-protocol-buffers)

This package lets you create encoders and decoders for working with
[ProtoBuf](https://developers.google.com/protocol-buffers) messages. Typically,
**you should not use this package directly** as the encoder and decoders can be
generated directly from `.proto` specification files.

## Motivation

> Protocol buffers are Google's language-neutral, platform-neutral, extensible
> mechanism for serializing structured data – think XML, but smaller, faster,
> and simpler. You define how you want your data to be structured once, then
> you can use special generated source code to easily write and read your
> structured data to and from a variety of data streams and using a variety of
> languages.

As Evan explained in his vision on
[data interchange](https://gist.github.com/evancz/1c5f2cf34939336ecb79b97bb89d9da6),
there are many ways to send information between clients and servers, like JSON
(optionally according to a specfication, like OpenAPI), GraphQL and the one
this package provides: **ProtoBuf**.

Where Elm spoils us with an excellent type system, we lose this safety at the
edges of our application when talking to servers. ProtoBuf requires you to
specify the interchange format explicity up front, making it **type-safe**. It
also:

* makes you **decouple code** for data exchange from your business logic,
  enabling your application to evolve over time;
* makes you send **fewer bytes** over the wire;
* allows you to **generate code** using the same data interchange format for
  your backend as well!
* is Google's **_lingua franca_** for data

This package provides an API to help converting between Elm values and bytes by
implementing the ProtoBuf specification. Ultimately, nobody should be writing
these encoders and decoders directly as the required code can be generated
automatically from `.proto` files. This is still a _work-in-progress_. Also
implementing [gRPC](https://grpc.io/docs/guides/index.html) could be an
interesting next step.

## Example

Given a ProtoBuf `.proto` file

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
import ProtoBuf.Decode as Decode
import ProtoBuf.Encode as Encode



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


encodePerson : Person -> Encode.Encoder
encodePerson person =
    Encode.message
        [ ( 1, Encode.string person.name )
        , ( 2, Encode.int32 person.id )
        , ( 3, Encode.string person.email )
        , ( 4, Encode.list encodePhoneNumber person.phone )
        ]


encodePhoneNumber : PhoneNumber -> Encode.Encoder
encodePhoneNumber phoneNumber =
    Encode.message
        [ ( 1, Encode.string phoneNumber.number )
        , ( 2, encodePhoneType phoneNumber.type_ )
        ]


encodePhoneType : PhoneType -> Encode.Encoder
encodePhoneType phoneType =
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
    Decode.message Person
        |> Decode.required 1 Decode.string
        |> Decode.required 2 Decode.int32
        |> Decode.optional 3 Decode.string ""
        |> Decode.repeated 4 phoneNumberDecoder


phoneNumberDecoder : Decode.Decoder PhoneNumber
phoneNumberDecoder =
    Decode.message PhoneNumber
        |> Decode.required 1 Decode.string
        |> Decode.optional 2 phoneTypeDecoder Home


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
```

## Known limitations

This packages aims to support both `proto2` and `proto3`. However, there are
some limitations.

* JavaScript uses 64-bit floating point
  [numbers](https://tc39.github.io/ecma262/#sec-ecmascript-language-types-number-type)
  for both integers and floats. Integers are only considered safe to use up to
  about 54 bits. Therefore all 64-bit integer variants are not supported by
  this package.

* Unknown fields are well-formed protocol buffer serialized data representing
  fields that the decoder does not recognize (for example when the field is
  removed from the `.proto` file but the server is not yet updated). Currently,
  these fields are ignored and hence are lost when re-serializing messages.

* The [any](https://developers.google.com/protocol-buffers/docs/proto3#any)
  type is currently not supported.

