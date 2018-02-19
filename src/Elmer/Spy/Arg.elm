module Elmer.Spy.Arg exposing
  ( Arg(..)
  , ArgValue(..)
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
  = SomeValue
  | FunctionArgValue


value : Arg -> ArgValue
value arg =
  case arg of
    StringArg str ->
      Value.cast str
    IntArg num ->
      Value.cast num
    FloatArg num ->
      Value.cast num
    BoolArg bool ->
      Value.cast bool
    TypedArg arg ->
      Value.cast arg
    FunctionArg ->
      FunctionArgValue
    AnyArg ->
      Value.cast never
    ArgThat _ ->
      Value.cast never


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
