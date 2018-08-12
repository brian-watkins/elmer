module Elmer.Runtime.Promise.Runner exposing
  ( run
  )

import Elmer.Runtime.Promise.Types exposing (..)
import Elmer.Runtime.Promise as Promise
import Json.Decode as Json exposing (Value)
import Elmer.Value as Value


emptyState : Promised msg
emptyState =
  { resolution = Promise.failWith "No resolution"
  , shouldDefer = False
  , commands = []
  }


run : Promise msg -> Promised msg
run promise =
  resolve promise emptyState


resolve : Promise msg -> Promised msg -> Promised msg
resolve promise promised =
  case promise of
    Complete resolution ->
      { promised | resolution = resolution }
    Continue continuation ->
      let
        promisedContinuation = resolve continuation.next promised
      in
        case promisedContinuation.resolution of
          Resolved value ->
            handleCallback value continuation.onResolve promisedContinuation
          Rejected value ->
            handleCallback value continuation.onReject promisedContinuation
          Aborted command ->
            promisedContinuation
    AndDo command next ->
      resolve next { promised | commands = command :: promised.commands }
    Defer deferred ->
      resolve deferred { promised | shouldDefer = True }


handleCallback : Value -> Maybe (Value -> Value) -> Promised msg -> Promised msg
handleCallback value maybeCallback promised =
  maybeCallback
    |> Maybe.map (applyCallback value promised)
    |> Maybe.withDefault promised


applyCallback : Value -> Promised msg -> (Value -> Value) -> Promised msg
applyCallback value promised callback =
  callback value
    |> Value.decode Promise.decoder
    |> unwrapOrFail
    |> resolveFor promised

resolveFor : Promised msg -> Promise msg -> Promised msg
resolveFor promised promise =
  resolve promise promised

unwrapOrFail : Result String a -> Promise msg
unwrapOrFail result =
  case result of
    Ok value ->
      Value.cast value
    Err msg ->
      "Error decoding promise: " ++ msg
        |> Promise.failWith
        |> Complete
