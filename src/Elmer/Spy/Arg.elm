module Elmer.Spy.Arg exposing
  ( Arg(..)
  , ArgValue
  , asString
  , value
  , decoder
  )

{-| Exposed for testing

@docs Arg, ArgValue, asString, value, decoder

-}

import Expect
import Json.Decode as Json exposing (Decoder)
import Elmer.Value.Native as Native
import Elmer.Internal as Internal


{-|
-}
type Arg
  = StringArg String
  | IntArg Int
  | FloatArg Float
  | BoolArg Bool
  | TypedArg ArgValue
  | FunctionArg
  | AnyArg
  | ArgThat (ArgValue -> Expect.Expectation)


{-|
-}
type ArgValue
  = ArgValue


{-|
-}
value : Arg -> Maybe ArgValue
value arg =
  case arg of
    StringArg str ->
      Just <| Native.cast str
    IntArg num ->
      Just <| Native.cast num
    FloatArg num ->
      Just <| Native.cast num
    BoolArg bool ->
      Just <| Native.cast bool
    TypedArg typed ->
      Just <| Native.cast typed
    FunctionArg ->
      Nothing
    AnyArg ->
      Just <| Native.cast never
    ArgThat _ ->
      Just <| Native.cast never


{-|
-}
asString : Arg -> String
asString arg =
  case arg of
    StringArg str ->
      "\"" ++ str ++ "\""
    IntArg num ->
      String.fromInt num
    FloatArg num ->
      String.fromFloat num
    BoolArg bool ->
       Internal.boolToString bool
    TypedArg typed ->
      Debug.toString typed
    FunctionArg ->
      "<FUNCTION>"
    AnyArg ->
      "<ANY>"
    ArgThat _ ->
      "<ARG_THAT>"


{-|
-}
decoder : Decoder Arg
decoder =
  Native.decoder
    |> Json.map (\arg -> (Native.nativeType arg, arg)) 
    |> Json.map (\(argType, val) ->
        case argType of
          "string" ->
            StringArg <| Native.cast val
          "int" ->
            IntArg <| Native.cast val
          "float" ->
            FloatArg <| Native.cast val
          "object" ->
            TypedArg <| Native.cast val
          "boolean" ->
            BoolArg <| Native.cast val
          "function" ->
            FunctionArg
          _ ->
            AnyArg
      )
