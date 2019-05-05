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

# Decode Elm values
@docs decode, decoder

# Custom Type Value Decoders
@docs constructor, firstArg, secondArg, thirdArg

# Data Structure Decoders
@docs tuple, list, dict

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

    Elmer.Value.decode constructor superTypeValue

This will result in: `Ok "FunType"`

It can be useful to combine this function with `Json.andThen` to decide which decoder to 
provide for a value:

    constructor
      |> Json.andThen (\ctor ->
        case ctor of
          "FunType" -> funTypeDecoder
          "AwesomeType" -> awesomeTypeDecoder
          _ -> Json.fail "Not a SuperType"
      )

-}
constructor : Json.Decoder String
constructor =
  Json.field "$" Json.string


{-| Decode an Elm value.

Since Elm values are JS objects under the hood, you can use any JSON decoder here.
However, you'll probably want to take advantage of the special decoders provided in this module, as
they encapsulate some of the details about how Elm values are represented in JS. 

Use `Elmer.Value.log` to help you discern what you might want to decode.
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

    Elmer.Value.decode (firstArg Json.string) superTypeValue

This will result in: `Ok "Bowling"`.

-}
firstArg : Json.Decoder a -> Json.Decoder v
firstArg =
  Json.field "a" << Json.map Native.unwrap


{-| Decode the second argument of a Custom Type value.

Suppose there's a custom type like so:

    type SuperType
      = CoolType String Int SomeRecord

Then, if `superTypeValue` is a `CoolType "Bowling" 28 myRecord`, you can decode its second argument like so.

    Elmer.Value.decode (secondArg Json.int) superTypeValue

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

    Elmer.Value.decode (thirdArg someRecordDecoder) superTypeValue

-}
thirdArg : Json.Decoder a -> Json.Decoder v
thirdArg =
  Json.field "c" << Json.map Native.unwrap


{-| Decode an Elm tuple.

Maybe there's an opaque type that hides a tuple:

    type HiddenTuple =
      HiddenTuple (String, Int)

Here's a decoder that allows you to inspect the tuple in a `HiddenTuple` value:

    Elmer.Value.firstArg <| 
      tuple Json.string Json.int

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

    Elmer.Value.firstArg decoder

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