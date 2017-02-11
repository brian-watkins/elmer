module Elmer.Command exposing
  ( fail
  , stub
  , defer
  , resolveDeferred
  , dummy
  , expectDummy
  , send
  , use
  , override
  , batchOverride
  )

import Elmer
import Elmer.Types exposing (..)
import Elmer.Runtime as Runtime
import Elmer.Printer exposing (..)
import Elmer.Platform as Platform exposing (PlatformOverride)
import Elmer.Command.Internal as InternalCommand
import Expect


batchOverride : List PlatformOverride -> PlatformOverride
batchOverride overrides =
  Platform.batchOverride overrides

override : (() -> a) -> (b -> c) -> PlatformOverride
override namingFunc overridingFunc =
  Platform.override namingFunc overridingFunc

use : List PlatformOverride -> (ComponentStateResult model msg -> ComponentStateResult model msg) -> ComponentStateResult model msg -> ComponentStateResult model msg
use overrides mapper =
  Platform.mapWithOverrides "commands" overrides (\componentState ->
    CurrentState componentState
      |> mapper
  )

fail : String -> Cmd msg
fail message =
  Platform.toCmd "Elmer_Fail" message

stub : msg -> Cmd msg
stub message =
  Platform.toCmd "Elmer_Stub" message

dummy : String -> Cmd msg
dummy identifier =
  InternalCommand.mapState <|
    updateComponentStateWithDummyCommand identifier

updateComponentStateWithDummyCommand : String -> HtmlComponentState model msg -> HtmlComponentState model msg
updateComponentStateWithDummyCommand identifier componentState =
  { componentState | dummyCommands = identifier :: componentState.dummyCommands }

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

defer : Cmd msg -> Cmd msg
defer command =
  InternalCommand.mapState <|
    updateComponentStateWithDeferredCommand command

updateComponentStateWithDeferredCommand : Cmd msg -> HtmlComponentState model msg -> HtmlComponentState model msg
updateComponentStateWithDeferredCommand command componentState =
  { componentState | deferredCommands = command :: componentState.deferredCommands }


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
