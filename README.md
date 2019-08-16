# Protocol Buffers [![Build Status](https://travis-ci.org/eriktim/elm-protocol-buffers.svg?branch=master)](https://travis-ci.org/eriktim/elm-protocol-buffers)

This package lets you create encoders and decoders for (de)serializing data
according to the [Protobuf](https://developers.google.com/protocol-buffers)
specification. Typically, **you should not use this package directly** as the
required encoders and decoders can be generated directly from `.proto`
specification files.

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
by implementing Protocol buffers. As Protocol buffers is only about
(de)serializing data, this package can be used with any compatible
transportation layer on top of it, e.g.
[gRPC](https://grpc.io/docs/guides/index.html) or
[RSocket](http://rsocket.io/). 

## Extensive Example

Given a Protobuf `.proto` file

```protobuf
syntax = "proto3"

message Person {
  string name = 1;
  int32 id = 2;
  string email = 3;

  repeated PhoneNumber phone = 4;
}

message PhoneNumber {
  string number = 1;
  PhoneType type = 2;
}

enum PhoneType {
  MOBILE = 0;
  HOME = 1;
  WORK = 2;
}
```

this package handles converting between `Person` and `Bytes` values:

```elm
import Protobuf.Codec as Codec
import Protobuf.Message as Message



-- MODEL


type alias Person =
    { name : String
    , id : Int
    , email : String
    , phone : List (Message.Message PhoneNumber)
    }


type alias PhoneNumber =
    { number : String
    , type_ : Maybe PhoneType
    }


type PhoneType
    = Mobile
    | Home
    | Work



-- ENCODE


personCodec : Codec.Codec (Message.Message Person)
personCodec =
    Codec.builder Person
        |> Codec.field 1 Codec.string .name
        |> Codec.field 2 Codec.int32 .id
        |> Codec.field 3 Codec.string .email
        |> Codec.repeated 4 phoneNumberCodec .phone
        |> Codec.build


phoneNumberCodec : Codec.Codec (Message.Message PhoneNumber)
phoneNumberCodec =
    Codec.builder PhoneNumber
        |> Codec.field 1 Codec.string .number
        |> Codec.enum 2 phoneTypeValues .type_
        |> Codec.build


phoneTypeValues : List ( Int, PhoneType )
phoneTypeValues =
    [ ( 0, Mobile )
    , ( 1, Home )
    , ( 2, Work )
    ]
```

## Known Limitations

This packages aims to support both `proto2` and `proto3`. However, there are
some limitations:

* JavaScript uses 64-bit floating point
  [numbers](https://tc39.github.io/ecma262/#sec-ecmascript-language-types-number-type)
  for both integers and floats. Integers are only considered safe to use up to
  about 54 bits. Therefore **all 64-bit integer variants are not supported** by
  this package.

* [extensions](https://developers.google.com/protocol-buffers/docs/proto#extensions)
  are currently **not supported**.

