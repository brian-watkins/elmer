module Elmer.Command exposing
  ( failureCommand
  , messageCommand
  , deferredCommand
  , mockCommand
  , expectMock
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

messageCommand : msg -> Cmd msg
messageCommand message =
  Native.Helpers.toCmd "Elmer_Message" message

mockCommand : String -> Cmd msg
mockCommand identifier =
  Native.Helpers.toCmd "Elmer_Mock" identifier

expectMock : String -> ComponentStateResult model msg -> Expect.Expectation
expectMock expectedIdentifier =
  Elmer.mapToExpectation (\componentState ->
    let
      mockCommands = List.filter (\identifier -> identifier == expectedIdentifier) componentState.mockCommands
    in
      if List.isEmpty mockCommands then
        Expect.fail (format [message "No mock commands sent with identifier" expectedIdentifier])
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
