module Elmer.CommandTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Command as Command
import Elmer
import Elmer.Types exposing (..)
import Elmer.TestApps.SimpleTestApp as App
import Elmer.TestApps.MessageTestApp as MessageApp
import Elmer.TestApps.ClickTestApp as ClickApp
import Elmer.Html.Matchers as Matchers
import Elmer.Printer exposing (..)
import Elmer.Html as Markup
import Task

all : Test
all =
  describe "Command Tests"
  [ elmerFailureCommandTest
  , elmerStubbedCommandTest
  , resolveDeferredCommandsTest
  , sendCommandTest
  , dummyCommandTests
  , useTests
  ]

elmerFailureCommandTest : Test
elmerFailureCommandTest =
  describe "when the Component Failure task is processed"
  [ test "it causes an upstream failure" <|
    \() ->
      let
        initialState = Elmer.componentState App.defaultModel App.view App.update
        result = Command.send (Command.fail "You failed!") initialState
      in
        Expect.equal (UpstreamFailure "You failed!") result
  ]

elmerStubbedCommandTest : Test
elmerStubbedCommandTest =
  describe "when the stubbed command is processed"
  [ test "it sends the message" <|
    \() ->
      let
        initialState = Elmer.componentState MessageApp.defaultModel MessageApp.view MessageApp.update
        msg = MessageApp.RenderFirstMessage "Hey this is the message!"
      in
        Command.send (Command.stub msg) initialState
          |> Markup.find "#first-message"
          |> Markup.expectNode (Matchers.hasText "Hey this is the message!")
  ]

resolveDeferredCommandsTest : Test
resolveDeferredCommandsTest =
  describe "resolveDeferredCommands"
  [ describe "when there is an upstream failure"
    [ test "it returns the error" <|
      \() ->
        let
          initialState = UpstreamFailure "You failed!"
          state = Command.resolveDeferred initialState
        in
          Expect.equal (UpstreamFailure "You failed!") state
    ]
  , describe "when there are no deferred commands"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Command.resolveDeferred initialState
            |> Expect.equal (UpstreamFailure "No deferred commands found")
    ]
  , let
      initialState = Elmer.componentState ClickApp.defaultModel ClickApp.view ClickApp.update
      deferredClickCommand = Command.stub ClickApp.DoClick
                              |> Command.defer
      state = Command.send deferredClickCommand initialState
                |> Command.send deferredClickCommand
                |> Command.send deferredClickCommand
    in
      describe "when there are deferred commands"
        [ test "it doesn't process deferred commands immediately" <|
          \() ->
            Markup.find "#click-counter" state
              |> Markup.expectNode (Matchers.hasText "0 clicks!")
        , let
            resolvedCommandsState = Command.resolveDeferred state
          in
            describe "when the deferred commands are resolved"
            [ test "it processes the deferred commands" <|
              \() ->
                Markup.find "#click-counter" resolvedCommandsState
                  |> Markup.expectNode (Matchers.hasText "3 clicks!")
            , test "it clears the deferred commands" <|
              \() ->
                Command.resolveDeferred resolvedCommandsState
                  |> Expect.equal (UpstreamFailure "No deferred commands found")
            ]
        ]
  ]

sendCommandTest =
  describe "send"
  [ describe "when there is an upstream failure"
    [ test "it passes on the error" <|
      \() ->
        let
          initialState = UpstreamFailure "upstream failure"
        in
          Command.send Cmd.none initialState
            |> Expect.equal initialState
    ]
  , describe "when there is no upstream failure"
    [ test "it executes the command and updates the component state" <|
        \() ->
          let
            initialState = Elmer.componentState MessageApp.defaultModel MessageApp.view MessageApp.update
            result = Command.send (Command.stub (MessageApp.RenderFirstMessage "Did it!")) initialState
          in
            case result of
              CurrentState updatedState ->
                Expect.equal updatedState.model.firstMessage "Did it!"
              UpstreamFailure msg ->
                Expect.fail msg
    ]
  ]


dummyCommandTests : Test
dummyCommandTests =
  describe "expectDummy"
  [ describe "when there is an upstream failure"
    [ test "it shows the failure" <|
      \() ->
        let
          state = UpstreamFailure "You Failed!"
          result = Command.expectDummy "someCommand" state
        in
          Expect.equal (Expect.fail "You Failed!") result
    ]
  , describe "when no dummy commands with the identifier are found"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
          dummyCommand = Command.dummy "someCommand"
        in
          Command.send dummyCommand initialState
            |> Command.expectDummy "fakeCommand"
            |> Expect.equal (Expect.fail (format [message "No dummy commands sent with identifier" "fakeCommand"]))
    ]
  , describe "when dummy commands with the identifier are found"
    [ test "it passes" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
          dummyCommand = Command.dummy "fakeCommand"
        in
          Command.send dummyCommand initialState
            |> Command.expectDummy "fakeCommand"
            |> Expect.equal Expect.pass
    ]
  ]

useTests : Test
useTests =
  describe "use"
  [ describe "when there is an upstream failure"
    [ test "it fails" <|
      \() ->
        let
          initialState = UpstreamFailure "You failed!"
          override = Command.override (\_ -> Task.perform) (\_ _ -> Command.dummy "dummy")
        in
          Command.use [ override ] (Markup.find "#root") initialState
            |> Expect.equal (UpstreamFailure "You failed!")
    ]
  , describe "when the override is not a function"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
          override = Command.override (\_ -> "Huh?") (\_ _ -> Command.dummy "dummy")
        in
          Command.use [ override ] (Markup.find "#root") initialState
            |> Expect.equal (UpstreamFailure "Failed to override commands!")
    ]
  ]
