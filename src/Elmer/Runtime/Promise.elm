module Elmer.Runtime.Promise exposing
  ( decoder
  , failWith
  )

import Json.Decode as Json exposing (Value)
import Elmer.Value as Value
import Elmer.Runtime.Command.Fail as Fail
import Elmer.Runtime.Promise.Types exposing (..)


decoder : Json.Decoder (Promise msg)
decoder =
  Json.oneOf
    [ Json.lazy (\_ -> Json.map Continue decodeContinuation)
    , Json.map Complete decodeResolution
    , Json.lazy (\_ -> decodeElmerPromise)
    ]


decodeContinuation : Json.Decoder (Continuation msg)
decodeContinuation =
  decodeConstructorAndThen <|
    \ctor ->
      case ctor of
        3 ->
          decodeAndThen
        4 ->
          decodeOnError
        unknown ->
          "Unknown decodeContinuation constructor: " ++ String.fromInt unknown
            |> Json.fail


decodeAndThen : Json.Decoder (Continuation msg)
decodeAndThen =
  Json.map3 Continuation
    nextPromiseDecoder
    (Json.map Just <| callbackDecoder)
    (Json.succeed Nothing)


decodeOnError : Json.Decoder (Continuation msg)
decodeOnError =
  Json.map3 Continuation
    nextPromiseDecoder
    (Json.succeed Nothing)
    (Json.map Just <| callbackDecoder)


nextPromiseDecoder : Json.Decoder (Promise msg)
nextPromiseDecoder =
  Json.field "d" (Json.lazy (\_ -> decoder))


callbackDecoder : Json.Decoder (Value -> Value)
callbackDecoder =
  Json.field "b" Value.decoder


decodeResolution : Json.Decoder (Resolution msg)
decodeResolution =
  decodeConstructorAndThen <|
    \ctor ->
      case ctor of
        0 -> -- Succeed
          Json.map Resolved valueDecoder
        1 -> -- Fail
          Json.map Rejected valueDecoder
        2 -> -- Native Binding
          failWith "Encountered a native task.\nStub any task-generating functions with Task.succeed or Task.fail as necessary."
            |> Json.succeed
        unknown ->
          Json.fail <| "Unknown Resolution constructor: " ++ String.fromInt unknown


valueDecoder : Json.Decoder Value
valueDecoder =
  Json.field "a" Value.decoder


decodeElmerPromise : Json.Decoder (Promise msg)
decodeElmerPromise =
  decodeConstructorAndThen <| 
    \ctor ->
      case ctor of
        1001 ->
          Json.map2 AndDo
            (Json.field "command" Value.decoder)
            (Json.field "task" (Json.lazy (\_ -> decoder)))
        1002 ->
          Json.map (Complete << Aborted) <| Json.field "command" Value.decoder
        1003 ->
          Json.map Defer
            (Json.field "task" (Json.lazy (\_ -> decoder)))
        unknown ->
          "Unknown andDo constructor: " ++ String.fromInt unknown
            |> Json.fail


decodeConstructorAndThen : (Int -> Json.Decoder a) -> Json.Decoder a
decodeConstructorAndThen generateDecoder =
  Json.field "$" Json.int
    |> Json.andThen generateDecoder


failWith : String -> Resolution msg
failWith message =
  Fail.with message
    |> Aborted
