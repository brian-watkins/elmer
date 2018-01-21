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
    , Json.lazy (\_ -> decodeAndDo)
    , Json.lazy (\_ -> decodeDefer)
    ]


decodeAndDo : Json.Decoder (Promise msg)
decodeAndDo =
  Json.field "ctor" Json.string
      |> Json.andThen (\ctor ->
        case ctor of
          "_Elmer_Task_andDo" ->
            Json.map2 AndDo
              (Json.field "command" (Json.map Value.cast Json.value))
              (Json.field "task" (Json.lazy (\_ -> decoder)))
          unknown ->
            "Unknown andDo constructor: " ++ unknown
              |> Json.fail
      )


decodeDefer : Json.Decoder (Promise msg)
decodeDefer =
  Json.field "ctor" Json.string
      |> Json.andThen (\ctor ->
        case ctor of
          "_Elmer_Task_defer" ->
            Json.map Defer
              (Json.field "task" (Json.lazy (\_ -> decoder)))
          unknown ->
            "Unknown defer constructor: " ++ unknown
              |> Json.fail
      )


decodeContinuation : Json.Decoder (Continuation msg)
decodeContinuation =
  Json.field "ctor" Json.string
    |> Json.andThen (\ctor ->
      case ctor of
        "_Task_onError" ->
          decodeOnError
        "_Task_andThen" ->
          decodeAndThen
        unknown ->
          "Unknown decodeContinuation constructor: " ++ unknown
            |> Json.fail
    )


decodeAndThen : Json.Decoder (Continuation msg)
decodeAndThen =
  Json.map3 Continuation
    (Json.field "task" (Json.lazy (\_ -> decoder)))
    (Json.map Just <| Json.map Value.cast <| Json.field "callback" Json.value)
    (Json.succeed Nothing)


decodeOnError : Json.Decoder (Continuation msg)
decodeOnError =
  Json.map3 Continuation
    (Json.field "task" (Json.lazy (\_ -> decoder)))
    (Json.succeed Nothing)
    (Json.map Just <| Json.map Value.cast <| Json.field "callback" Json.value)


decodeResolution : Json.Decoder (Resolution msg)
decodeResolution =
  Json.field "ctor" Json.string
    |> Json.andThen (\ctor ->
      case ctor of
        "_Task_succeed" ->
          Json.map Resolved valueDecoder
        "_Task_fail" ->
          Json.map Rejected valueDecoder
        "_Task_nativeBinding" ->
          failWith "Encountered a native task.\nStub any task-generating functions with Task.succeed or Task.fail as necessary."
            |> Json.succeed
        "_Elmer_Task_abort" ->
          Json.map Aborted <| Json.map Value.cast valueDecoder
        unknown ->
          Json.fail <| "Unknown Resolution constructor: " ++ unknown
    )


valueDecoder : Json.Decoder Value
valueDecoder =
  Json.field "value" Json.value


failWith : String -> Resolution msg
failWith message =
  Fail.with message
    |> Aborted
