module Elmer.Spy.Arg exposing
  ( Arg(..)
  , ArgValue
  , asString
  , value
  , decoder
  )

import Expect
import Json.Decode as Json exposing (Decoder)
import Elmer.Value as Value
import Elmer.Internal as Internal


type Arg
  = StringArg String
  | IntArg Int
  | FloatArg Float
  | BoolArg Bool
  | TypedArg ArgValue
  | FunctionArg
  | AnyArg
  | ArgThat (ArgValue -> Expect.Expectation)


type ArgValue
  = ArgValue


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
    TypedArg arg ->
      Just <| Value.cast arg
    FunctionArg ->
      Nothing
    AnyArg ->
      Just <| Value.cast never
    ArgThat _ ->
      Just <| Value.cast never


asString : Arg -> String
asString arg =
  case arg of
    StringArg str ->
      "\"" ++ str ++ "\""
    IntArg num ->
      toString num
    FloatArg num ->
      toString num
    BoolArg bool ->
       Internal.boolToString bool
    TypedArg arg ->
      toString arg
    FunctionArg ->
      "<FUNCTION>"
    AnyArg ->
      "<ANY>"
    ArgThat _ ->
      "<ARG_THAT>"


decoder : Decoder Arg
decoder =
  Json.map (\arg -> (Value.nativeType arg, arg)) Json.value
    |> Json.map (\(argType, value) ->
        case argType of
          "string" ->
            StringArg <| Value.cast value
          "int" ->
            IntArg <| Value.cast value
          "float" ->
            FloatArg <| Value.cast value
          "object" ->
            TypedArg <| Value.cast value
          "boolean" ->
            BoolArg <| Value.cast value
          "function" ->
            FunctionArg
          _ ->
            AnyArg
      )
