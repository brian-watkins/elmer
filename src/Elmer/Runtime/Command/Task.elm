module Elmer.Runtime.Command.Task exposing
  ( commandRunner
  )

import Elmer.Runtime.Intention as Intention
import Elmer.Runtime.Types exposing (..)
import Elmer.Value as Value
import Json.Decode as Json exposing (Value)
import Elmer.Context as Context exposing (Context)


type Promise
  = Complete Resolution
  | Continue Continuation

type alias Continuation =
  { next : Promise
  , onResolve : Maybe (Value -> Value)
  , onReject : Maybe (Value -> Value)
  }

type Resolution
  = Resolved Value
  | Rejected Value
  | Native


commandRunner : CommandRunner model subMsg msg
commandRunner command tagger =
  let
    taskResult =
      Intention.cmdValue command
        |> Value.mapArg (Value.decode decodePromise)
  in
    case taskResult of
      Ok promise ->
        case resolve promise of
          Resolved promiseValue ->
            CommandSuccess (Context.update (tagger (Value.cast promiseValue)))
          Rejected failedValue ->
            CommandError "Encountered a task failure, but no error handler has been specified. This should not happen."
          Native ->
            CommandError "Encountered a real task. Use Elmer.Task.fake to stub any task-generating functions."
      Err msg ->
        CommandError <| "Error decoding Task: " ++ msg


resolve : Promise -> Resolution
resolve promise =
  case promise of
    Complete result ->
      result
    Continue continuation ->
      case resolve continuation.next of
        Resolved value ->
          doCallback Resolved value continuation.onResolve
        Rejected value ->
          doCallback Rejected value continuation.onReject
        Native ->
          Native


doCallback : (Value -> Resolution) -> Value -> Maybe (Value -> Value) -> Resolution
doCallback mapper value maybeCallback =
  case maybeCallback of
    Just callback ->
      callback value
        |> Value.decode decodePromise
        |> unwrapOrFail
        |> resolve
    Nothing ->
      mapper value


unwrapOrFail : Result String a -> a
unwrapOrFail result =
  case result of
    Ok value ->
      value
    Err msg ->
      Debug.crash msg


decodePromise : Json.Decoder Promise
decodePromise =
  Json.oneOf
    [ Json.lazy (\_ -> Json.map Continue decodeContinuation)
    , Json.map Complete decodeResolution
    ]


decodeContinuation : Json.Decoder Continuation
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


decodeAndThen : Json.Decoder Continuation
decodeAndThen =
  Json.map3 Continuation
    (Json.field "task" (Json.lazy (\_ -> decodePromise)))
    (Json.map Just <| Json.map Value.cast <| Json.field "callback" Json.value)
    (Json.succeed Nothing)


decodeOnError : Json.Decoder Continuation
decodeOnError =
  Json.map3 Continuation
    (Json.oneOf
      [ Json.map Complete <| Json.field "task" decodeResolution
      , Json.at [ "task", "task" ] (Json.lazy (\_ -> decodePromise))
      ]
    )
    (Json.maybe <| Json.map Value.cast <| Json.at [ "task", "callback" ] Json.value)
    (Json.map Just <| Json.map Value.cast <| Json.field "callback" Json.value)


decodeResolution : Json.Decoder Resolution
decodeResolution =
  Json.field "ctor" Json.string
    |> Json.andThen (\ctor ->
      case ctor of
        "_Task_succeed" ->
          Json.map Resolved <| Json.field "value" Json.value
        "_Task_fail" ->
          Json.map Rejected <| Json.field "value" Json.value
        "_Task_nativeBinding" ->
          Json.succeed Native
        unknown ->
          Json.fail <| "Unknown Resolution constructor: " ++ unknown
    )
