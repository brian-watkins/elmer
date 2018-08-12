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
import Elmer.Value as Value
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
      Just <| Value.cast str
    IntArg num ->
      Just <| Value.cast num
    FloatArg num ->
      Just <| Value.cast num
    BoolArg bool ->
      Just <| Value.cast bool
    TypedArg typed ->
      Just <| Value.cast typed
    FunctionArg ->
      Nothing
    AnyArg ->
      Just <| Value.cast never
    ArgThat _ ->
      Just <| Value.cast never


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
  Value.decoder
    |> Json.map (\arg -> (Value.nativeType arg, arg)) 
    |> Json.map (\(argType, val) ->
        case argType of
          "string" ->
            StringArg <| Value.cast val
          "int" ->
            IntArg <| Value.cast val
          "float" ->
            FloatArg <| Value.cast val
          "object" ->
            TypedArg <| Value.cast val
          "boolean" ->
            BoolArg <| Value.cast val
          "function" ->
            FunctionArg
          _ ->
            AnyArg
      )
