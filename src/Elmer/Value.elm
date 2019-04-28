module Elmer.Value exposing
  ( decode
  , decoder
  , constructor
  , firstArg
  , secondArg
  , thirdArg
  , tuple
  , list
  , dict
  , log
  )

{-| Inspect Elm values at runtime.

These functions are most helpful for inspecting the value of an opaque type during a test.

# Inspect the Value of a Custom Type
@docs constructor, firstArg, secondArg, thirdArg

# Inspect a Data Structure
@docs tuple, list, dict

# Decode Elm values
@docs decode, decoder

# Debugging
@docs log

-}

import Json.Decode as Json
import Dict exposing (Dict)
import Elmer.Value.Native as Native exposing (decoder)


{-| Decode the constructor of a Custom Type value.

Suppose there's a custom type like so:

    type SuperType
      = FunType String
      | AwesomeType Int

Then, if `superTypeValue` is a `FunType "Bowling"`, you can decode its constructor like so.

    Value.decode Value.constructor superTypeValue

This will result in: `Ok "FunType"`

-}
constructor : Json.Decoder String
constructor =
  Json.field "$" Json.string


{-| Decode an Elm value.

Since Elm values are JS objects under the hood, you can use any JSON decoder here.
However, you'll probably want to take advantage of the special decoders provided in this module, as
they encapsulate some of the details about how Elm values are represented in JS. 

Use `Value.log` to help you discern what you might want to decode.
-}
decode : Json.Decoder a -> v -> Result Json.Error b
decode =
  Native.decode


{-| Decode the first argument of a Custom Type value.

Suppose there's a custom type like so:

    type SuperType
      = FunType String
      | AwesomeType Int

Then, if `superTypeValue` is a `FunType "Bowling"`, you can decode its first argument like so.

    Value.decode (Value.firstArg Json.string) superTypeValue

This will result in: `Ok "Bowling"`.

You might want to use this in conjunction with `Value.constructor` to decide which
argument decoder to use. Here's a decoder to get the first argument for any `SuperType` value:

    Value.constructor
      |> Json.andThen (\ctor ->
        case ctor of
          "FunType" ->
            Value.firstArg Json.string
          "AwesomeType" ->
            Value.firstArg Json.int
          _ ->
            Json.fail "Not a SuperType value"
      )

-}
firstArg : Json.Decoder a -> Json.Decoder v
firstArg =
  Json.field "a" << Json.map Native.unwrap


{-| Decode the second argument of a Custom Type value.

Suppose there's a custom type like so:

    type SuperType
      = CoolType String Int SomeRecord

Then, if `superTypeValue` is a `CoolType "Bowling" 28 myRecord`, you can decode its second argument like so.

    Value.decode (Value.secondArg Json.int) superTypeValue

This will result in: `Ok 28`.

-}
secondArg : Json.Decoder a -> Json.Decoder v
secondArg =
  Json.field "b" << Json.map Native.unwrap


{-| Decode the third argument of a Custom Type value.

Suppose there's a custom type like so:

    type SuperType
      = CoolType String Int SomeRecord

Then, if `superTypeValue` is a `CoolType "Bowling" 28 myRecord`, you can decode its third argument like so.

    Value.decode (Value.thirdArg someRecordDecoder) superTypeValue

-}
thirdArg : Json.Decoder a -> Json.Decoder v
thirdArg =
  Json.field "c" << Json.map Native.unwrap


{-| Decode an Elm tuple.

Maybe there's an opaque type that hides a tuple:

    type HiddenTuple =
      HiddenTuple (String, Int)

Here's a decoder that allows you to inspect the tuple in a `HiddenTuple` value:

    Value.firstArg <| 
      Value.tuple Json.string Json.int

-}
tuple : Json.Decoder a -> Json.Decoder b -> Json.Decoder (a, b)
tuple aDecoder bDecoder =
  Json.map2 Tuple.pair
    (Json.field "a" aDecoder)
    (Json.field "b" bDecoder)


{-| Decode an Elm list.

-}
list : Json.Decoder a -> Json.Decoder (List a)
list itemDecoder =
  constructor
    |> Json.andThen (\ctor ->
      case ctor of
        "[]" ->
          Json.succeed []
        "::" ->
          Json.field "a" itemDecoder
            |> Json.andThen (\element ->
              Json.field "b" (list itemDecoder)
                |> Json.map (\others -> element :: others)
            )
        unknown ->
          Json.fail <| "Not a list constructor: " ++ unknown
    )


{-| Decode an Elm dict.

-}
dict : Json.Decoder a -> Json.Decoder (Dict String a)
dict valueDecoder =
  constructor
    |> Json.andThen (\ctor ->
      case ctor of
        "RBEmpty_elm_builtin" ->
          Json.succeed Dict.empty
        "RBNode_elm_builtin" ->
          Json.map2 Dict.singleton (Json.field "b" Json.string) (Json.field "c" valueDecoder)
            |> Json.andThen (\current ->
                Json.map2 (\left right ->
                  Dict.union left right
                    |> Dict.union current
                )
                (Json.field "d" <| dict valueDecoder)
                (Json.field "e" <| dict valueDecoder)
              )
        unknown ->
          Json.fail <| "Not a Dict constructor: " ++ unknown
    )


{-| Decode an arbitrary value.

Suppose there's an opaque type that hides a function:

    type HiddenFunction =
      HiddenFunction (String -> Int)

You could decode the function like so:

    Value.firstArg Value.decoder

By using this decoder on a `HiddenFunction` value, you can gain access
to and use the function just like you'd expect.

-}
decoder : Json.Decoder a
decoder =
  Native.decoder


{-| Log an Elm Value.

This function works like `Debug.log` but prints more details
about how the value is represented in JS.

-}
log : String -> v -> v
log =
  Elm.Kernel.Value.print