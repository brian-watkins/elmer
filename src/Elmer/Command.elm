module Elmer.Command exposing
  ( failureCommand
  , messageCommand
  , deferredCommand
  , resolveDeferred
  , send
  )

import Elmer
import Elmer.Types exposing (..)
import Elmer.Runtime as Runtime

failureCommand : String -> Cmd msg
failureCommand message =
  Native.Helpers.toCmd "Elmer_Failure" message

messageCommand : msg -> Cmd msg
messageCommand message =
  Native.Helpers.toCmd "Elmer_Message" message

deferredCommand : Cmd msg -> Cmd msg
deferredCommand command =
  Native.Helpers.toCmd "Elmer_Deferred" command

resolveDeferred : ComponentStateResult model msg -> ComponentStateResult model msg
resolveDeferred =
  Elmer.map (\componentState ->
    if List.isEmpty componentState.deferredCommands then
      UpstreamFailure "No deferred commands found"
    else
      let
        deferredCommands = Cmd.batch componentState.deferredCommands
        updatedComponentState = { componentState | deferredCommands = [] }
      in
        Runtime.performCommand deferredCommands updatedComponentState
          |> asComponentStateResult
  )

send : Cmd msg -> ComponentStateResult model msg -> ComponentStateResult model msg
send command =
    Elmer.map (\state ->
      Runtime.performCommand command state
        |> asComponentStateResult
    )


asComponentStateResult : Result String (HtmlComponentState model msg) -> ComponentStateResult model msg
asComponentStateResult commandResult =
  case commandResult of
    Ok updatedComponentState ->
      CurrentState updatedComponentState
    Err message ->
      UpstreamFailure message
