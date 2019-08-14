module WellKnownTypesTest exposing (suite)

import Bytes
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, float, int, list, maybe, string)
import Helper exposing (..)
import Hex
import Protobuf.Codec as Codec
import Test exposing (..)


suite : Test
suite =
    describe "Should successfully transmit"
        []



{-
           [ fuzz any "Any" <|
               transmit toAnyEncoder anyDecoder
           , fuzz api "Api" <|
               transmit toApiEncoder apiDecoder
           , fuzz duration "Duration" <|
               transmit toDurationEncoder durationDecoder
           , fuzz empty "Empty" <|
               transmit toEmptyEncoder emptyDecoder
           , fuzz fieldMask "Field Mask" <|
               transmit toFieldMaskEncoder fieldMaskDecoder
           , fuzz (struct 2) "Struct" <|
               transmit toStructEncoder structDecoder
           , fuzz timestamp "Timestamp" <|
               transmit toTimestampEncoder timestampDecoder
           , describe "Types"
               [ fuzz type_ "Type" <|
                   transmit toTypeEncoder typeDecoder
               , fuzz enum "Enum" <|
                   transmit toEnumEncoder enumDecoder
               ]
           , describe "Wrappers"
               [ fuzz doubleValue "DoubleValue" <|
                   transmit toDoubleValueEncoder doubleValueDecoder
               , fuzz floatValue "FloatValue" <|
                   transmit toFloatValueEncoder floatValueDecoder
               , fuzz int32Value "Int32Value" <|
                   transmit toInt32ValueEncoder int32ValueDecoder
               , fuzz uint32Value "UInt32Value" <|
                   transmit toUInt32ValueEncoder uInt32ValueDecoder
               , fuzz boolValue "BoolValue" <|
                   transmit toBoolValueEncoder boolValueDecoder
               , fuzz stringValue "StringValue" <|
                   transmit toStringValueEncoder stringValueDecoder
               , fuzz bytesValue "BytesValue" <|
                   transmit toBytesValueEncoder bytesValueDecoder
               ]
           ]



   -- FUZZERS


   any : Fuzzer Any
   any =
       Fuzz.map2 Any string bytes


   api : Fuzzer Api
   api =
       Fuzz.map3 (\api_ name_ methods_ -> { api_ | name = name_, methods = methods_ })
           (Fuzz.map5 (Api "" []) (list option) string (maybe sourceContext) (list mixin) syntax)
           string
           (list method)


   boolValue : Fuzzer BoolValue
   boolValue =
       Fuzz.map BoolValue bool


   bytesValue : Fuzzer BytesValue
   bytesValue =
       Fuzz.map BytesValue bytes


   cardinality : Fuzzer Cardinality
   cardinality =
       Fuzz.frequency
           [ ( 1, Fuzz.constant CardinalityUnknown )
           , ( 1, Fuzz.constant CardinalityOptional )
           , ( 1, Fuzz.constant CardinalityRequired )
           , ( 1, Fuzz.constant CardinalityRepeated )
           , ( 1, Fuzz.constant (CardinalityUnrecognized_ -1) )
           ]


   doubleValue : Fuzzer DoubleValue
   doubleValue =
       Fuzz.map DoubleValue float


   duration : Fuzzer Duration
   duration =
       Fuzz.map2 Duration int32 int32


   empty : Fuzzer Empty
   empty =
       Fuzz.constant Empty


   enum : Fuzzer Enum
   enum =
       Fuzz.map5 Enum string (list enumValue) (list option) (maybe sourceContext) syntax


   enumValue : Fuzzer EnumValue
   enumValue =
       Fuzz.map3 EnumValue string int32 (list option)


   field : Fuzzer Field
   field =
       Fuzz.map2 (\field_ typeUrl_ -> { field_ | typeUrl = typeUrl_ })
           (Fuzz.map5 (\field_ kind_ cardinality_ number_ name_ -> { field_ | kind = kind_, cardinality = cardinality_, number = number_, name = name_ })
               (Fuzz.map5 (Field TypeUnknown CardinalityUnknown 0 "" "") int32 bool (list option) string string)
               kind
               cardinality
               int32
               string
           )
           string


   fieldMask : Fuzzer FieldMask
   fieldMask =
       Fuzz.map FieldMask (list string)


   floatValue : Fuzzer FloatValue
   floatValue =
       Fuzz.map FloatValue float32


   int32Value : Fuzzer Int32Value
   int32Value =
       Fuzz.map Int32Value int32


   kind : Fuzzer Kind
   kind =
       Fuzz.frequency
           [ ( 1, Fuzz.constant TypeUnknown )
           , ( 1, Fuzz.constant TypeDouble )
           , ( 1, Fuzz.constant TypeFloat )
           , ( 1, Fuzz.constant TypeInt64 )
           , ( 1, Fuzz.constant TypeUint64 )
           , ( 1, Fuzz.constant TypeInt32 )
           , ( 1, Fuzz.constant TypeFixed64 )
           , ( 1, Fuzz.constant TypeFixed32 )
           , ( 1, Fuzz.constant TypeBool )
           , ( 1, Fuzz.constant TypeString )
           , ( 1, Fuzz.constant TypeGroup )
           , ( 1, Fuzz.constant TypeMessage )
           , ( 1, Fuzz.constant TypeBytes )
           , ( 1, Fuzz.constant TypeUint32 )
           , ( 1, Fuzz.constant TypeEnum )
           , ( 1, Fuzz.constant TypeSfixed32 )
           , ( 1, Fuzz.constant TypeSfixed64 )
           , ( 1, Fuzz.constant TypeSint32 )
           , ( 1, Fuzz.constant TypeSint64 )
           , ( 1, Fuzz.constant (KindUnrecognized_ -1) )
           ]


   uint32Value : Fuzzer UInt32Value
   uint32Value =
       Fuzz.map UInt32Value uint32


   kindType : Int -> Fuzzer KindType
   kindType i =
       if i <= 0 then
           Fuzz.constant (KindBoolValue False)

       else
           Fuzz.frequency
               [ ( 1, Fuzz.map KindNullValue nullValue )
               , ( 1, Fuzz.map KindNumberValue float32 )
               , ( 1, Fuzz.map KindStringValue string )
               , ( 1, Fuzz.map KindBoolValue bool )
               , ( 1, Fuzz.map KindStructValue (struct <| i - 1) )
               , ( 1, Fuzz.map KindListValue (listValue <| i - 1) )
               ]


   listValue : Int -> Fuzzer ListValue
   listValue i =
       Fuzz.map (ListValue << ListValueValues) (list <| value i)


   method : Fuzzer Method
   method =
       Fuzz.map3 (\method_ name_ requestTypeUrl_ -> { method_ | name = name_, requestTypeUrl = requestTypeUrl_ })
           (Fuzz.map5 (Method "" "") bool string bool (list option) syntax)
           string
           string


   mixin : Fuzzer Mixin
   mixin =
       Fuzz.map2 Mixin string string


   nullValue : Fuzzer NullValue
   nullValue =
       Fuzz.frequency
           [ ( 1, Fuzz.constant NullValue )
           , ( 1, Fuzz.constant (NullValueUnrecognized_ -1) )
           ]


   option : Fuzzer Option
   option =
       Fuzz.map2 Option string (maybe any)


   sourceContext : Fuzzer SourceContext
   sourceContext =
       Fuzz.map SourceContext string


   stringValue : Fuzzer StringValue
   stringValue =
       Fuzz.map StringValue string


   struct : Int -> Fuzzer Struct
   struct i =
       Fuzz.map (Struct << StructFields) (dict string (maybe <| value i))


   syntax : Fuzzer Syntax
   syntax =
       Fuzz.frequency
           [ ( 1, Fuzz.constant SyntaxProto2 )
           , ( 1, Fuzz.constant SyntaxProto3 )
           ]


   timestamp : Fuzzer Timestamp
   timestamp =
       Fuzz.map2 Timestamp int32 int32


   type_ : Fuzzer Type
   type_ =
       Fuzz.map2 (\type__ name_ -> { type__ | name = name_ })
           (Fuzz.map5 (Type "") (list field) (list string) (list option) (maybe sourceContext) syntax)
           string


   value : Int -> Fuzzer Value
   value i =
       Fuzz.map (Value << ValueKind) (maybe (kindType i))
-}
