module Elmer.Runtime.Command.Task exposing
  ( commandRunner
  , name
  )

import Elmer.Runtime.Intention as Intention
import Elmer.Runtime.Types exposing (..)
import Elmer.Runtime.Command.Fail as Fail
import Elmer.Runtime.Command.Defer as Defer
import Elmer.Runtime.Command.Stub as Stub
import Elmer.Value as Value
import Elmer.Runtime.Promise as Promise
import Elmer.Runtime.Promise.Runner as PromiseRunner
import Elmer.Runtime.Promise.Types exposing (..)


name : String
name =
  "Task"


commandRunner : CommandRunner model subMsg msg
commandRunner command tagger =
  let
    taskResult =
      Intention.cmdValue command
        |> Value.mapArg (Value.decode Promise.decoder)
  in
    case taskResult of
      Ok promise ->
        CommandSuccess <|
          \context ->
            ( context
            , PromiseRunner.run promise
                |> promiseCommand tagger
            )
      Err msg ->
        CommandError <|
          "Error decoding Task: " ++ msg


promiseCommand : (subMsg -> msg) -> Promised msg -> Cmd msg
promiseCommand tagger promised =
  (toCommand tagger promised.resolution) :: promised.commands
    |> Cmd.batch
    |> deferIf promised.shouldDefer


deferIf : Bool -> Cmd msg -> Cmd msg
deferIf shouldDefer command =
  if shouldDefer then
    Defer.with command
  else
    command


toCommand : (subMsg -> msg) -> Resolution msg -> Cmd msg
toCommand tagger resolution =
  case resolution of
    Resolved promiseValue ->
      Value.cast promiseValue
        |> tagger
        |> Stub.with
    Rejected _ ->
      "Encountered a task failure, but no error handler has been specified. This should not happen."
        |> Fail.with
    Aborted command ->
      command
