module Elmer.CommandTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Command
import Elmer
import Elmer.Types exposing (..)
import Elmer.TestApps.SimpleTestApp as App
import Elmer.TestApps.MessageTestApp as MessageApp
import Elmer.TestApps.ClickTestApp as ClickApp
import Elmer.Event as Event
import Elmer.Matchers as Matchers

all : Test
all =
  describe "Command Tests"
  [ elmerFailureCommandTest
  , elmerMessageCommandTest
  , resolveDeferredCommandsTest
  ]

elmerFailureCommandTest : Test
elmerFailureCommandTest =
  describe "when the Component Failure task is processed"
  [ test "it causes an upstream failure" <|
    \() ->
      let
        initialState = Elmer.componentState App.defaultModel App.view App.update
        result = Event.sendCommand (Elmer.Command.failureCommand "You failed!") initialState
      in
        Expect.equal (UpstreamFailure "You failed!") result
  ]

elmerMessageCommandTest : Test
elmerMessageCommandTest =
  describe "when the message command is processed"
  [ test "it sends the message" <|
    \() ->
      let
        initialState = Elmer.componentState MessageApp.defaultModel MessageApp.view MessageApp.update
        msg = MessageApp.RenderFirstMessage "Hey this is the message!"
      in
        Event.sendCommand (Elmer.Command.messageCommand msg) initialState
          |> Elmer.find "#first-message"
          |> Elmer.expectNode (Matchers.hasText "Hey this is the message!")
  ]

resolveDeferredCommandsTest : Test
resolveDeferredCommandsTest =
  describe "resolveDeferredCommands"
  [ describe "when there is an upstream failure"
    [ test "it returns the error" <|
      \() ->
        let
          initialState = UpstreamFailure "You failed!"
          state = Elmer.Command.resolveDeferred initialState
        in
          Expect.equal (UpstreamFailure "You failed!") state
    ]
  , describe "when there are no deferred commands"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Elmer.Command.resolveDeferred initialState
            |> Expect.equal (UpstreamFailure "No deferred commands found")
    ]
  , let
      initialState = Elmer.componentState ClickApp.defaultModel ClickApp.view ClickApp.update
      deferredClickCommand = Elmer.Command.messageCommand ClickApp.DoClick
                              |> Elmer.Command.deferredCommand
      state = Event.sendCommand deferredClickCommand initialState
                |> Event.sendCommand deferredClickCommand
                |> Event.sendCommand deferredClickCommand
    in
      describe "when there are deferred commands"
        [ test "it doesn't process deferred commands immediately" <|
          \() ->
            Elmer.find "#click-counter" state
              |> Elmer.expectNode (Matchers.hasText "0 clicks!")
        , let
            resolvedCommandsState = Elmer.Command.resolveDeferred state
          in
            describe "when the deferred commands are resolved"
            [ test "it processes the deferred commands" <|
              \() ->
                Elmer.find "#click-counter" resolvedCommandsState
                  |> Elmer.expectNode (Matchers.hasText "3 clicks!")
            , test "it clears the deferred commands" <|
              \() ->
                Elmer.Command.resolveDeferred resolvedCommandsState
                  |> Expect.equal (UpstreamFailure "No deferred commands found")
            ]
        ]
  ]
