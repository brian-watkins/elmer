module Elmer.Command exposing
  ( failureCommand
  , stubbedCommand
  , deferredCommand
  , dummyCommand
  , expectDummy
  , resolveDeferred
  , send
  )

import Elmer
import Elmer.Types exposing (..)
import Elmer.Runtime as Runtime
import Elmer.Printer exposing (..)
import Expect

failureCommand : String -> Cmd msg
failureCommand message =
  Native.Helpers.toCmd "Elmer_Failure" message

stubbedCommand : msg -> Cmd msg
stubbedCommand message =
  Native.Helpers.toCmd "Elmer_Message" message

dummyCommand : String -> Cmd msg
dummyCommand identifier =
  Native.Helpers.toCmd "Elmer_Dummy" identifier

expectDummy : String -> ComponentStateResult model msg -> Expect.Expectation
expectDummy expectedIdentifier =
  Elmer.mapToExpectation (\componentState ->
    let
      dummyCommands = List.filter (\identifier -> identifier == expectedIdentifier) componentState.dummyCommands
    in
      if List.isEmpty dummyCommands then
        Expect.fail (format [message "No dummy commands sent with identifier" expectedIdentifier])
      else
        Expect.pass
  )

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
