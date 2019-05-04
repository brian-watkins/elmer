module Elmer.CommandTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.Command as Command
import Elmer
import Elmer.TestState as TestState exposing (..)
import Elmer.TestApps.SimpleTestApp as App
import Elmer.TestApps.MessageTestApp as MessageApp
import Elmer.TestApps.MouseTestApp as ClickApp
import Elmer.Html.Matchers as Matchers exposing (element, hasText)
import Elmer.Html.Selector as Sel exposing (by)
import Elmer.Message exposing (..)
import Elmer.Html as Markup
import Task


all : Test
all =
  Test.concat
  [ elmerFailureCommandTest
  , elmerStubbedCommandTest
  , resolveDeferredCommandsTest
  , sendCommandTest
  , dummyCommandTests
  ]

elmerFailureCommandTest : Test
elmerFailureCommandTest =
  describe "when the Component Failure task is processed"
  [ test "it causes an upstream failure" <|
    \() ->
      let
        initialState = Elmer.given App.defaultModel App.view App.update
        result = Command.send (\() -> Command.fail "You failed!") initialState
      in
        Expect.equal (TestState.failure "You failed!") result
  ]

elmerStubbedCommandTest : Test
elmerStubbedCommandTest =
  describe "when the stubbed command is processed"
  [ test "it sends the message" <|
    \() ->
      let
        initialState = Elmer.given MessageApp.defaultModel MessageApp.view MessageApp.update
        msg = MessageApp.RenderFirstMessage "Hey this is the message!"
      in
        Command.send (\() -> Command.fake msg) initialState
          |> Markup.target << by [ Sel.id "first-message" ]
          |> Markup.expect (element <| hasText "Hey this is the message!")
  ]

resolveDeferredCommandsTest : Test
resolveDeferredCommandsTest =
  describe "resolveDeferredCommands"
  [ describe "when there is an upstream failure"
    [ test "it returns the error" <|
      \() ->
        let
          initialState = TestState.failure "You failed!"
          state = Command.resolveDeferred initialState
        in
          Expect.equal (TestState.failure "You failed!") state
    ]
  , describe "when there are no deferred commands"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.given App.defaultModel App.view App.update
        in
          Command.resolveDeferred initialState
            |> Expect.equal (TestState.failure "No deferred commands found")
    ]
  , let
      initialState = Elmer.given ClickApp.defaultModel ClickApp.view ClickApp.update
      deferredClickCommandThunk = \() ->
        Command.fake ClickApp.DoClick
          |> Command.defer
      state = Command.send deferredClickCommandThunk initialState
                |> Command.send deferredClickCommandThunk
                |> Command.send deferredClickCommandThunk
    in
      describe "when there are deferred commands"
        [ test "it doesn't process deferred commands immediately" <|
          \() ->
            state
              |> Markup.target << by [ Sel.id "click-counter" ]
              |> Markup.expect (element <| hasText "0 clicks!")
        , let
            resolvedCommandsState = Command.resolveDeferred state
          in
            describe "when the deferred commands are resolved"
            [ test "it processes the deferred commands" <|
              \() ->
                resolvedCommandsState
                  |> Markup.target << by [ Sel.id "click-counter" ]
                  |> Markup.expect (element <| hasText "3 clicks!")
            , test "it clears the deferred commands" <|
              \() ->
                Command.resolveDeferred resolvedCommandsState
                  |> Expect.equal (TestState.failure "No deferred commands found")
            ]
        ]
  ]

sendCommandTest =
  describe "send"
  [ describe "when there is an upstream failure"
    [ test "it passes on the error" <|
      \() ->
        let
          initialState = TestState.failure "upstream failure"
        in
          Command.send (\() -> Cmd.none) initialState
            |> Expect.equal initialState
    ]
  , describe "when there is no upstream failure"
    [ test "it executes the command and updates the test state" <|
        \() ->
          Elmer.given MessageApp.defaultModel MessageApp.view MessageApp.update
            |> Command.send (\() -> Command.fake <| MessageApp.RenderFirstMessage "Did it!")
            |> Elmer.expectModel (\model ->
              Expect.equal model.firstMessage "Did it!"
            )
    ]
  ]


dummyCommandTests : Test
dummyCommandTests =
  describe "expectDummy"
  [ describe "when there is an upstream failure"
    [ test "it shows the failure" <|
      \() ->
        let
          state = TestState.failure "You Failed!"
          result = Command.expectDummy "someCommand" state
        in
          Expect.equal (Expect.fail "You Failed!") result
    ]
  , describe "when no dummy commands with the identifier are found"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.given App.defaultModel App.view App.update
          dummyCommand = Command.dummy "someCommand"
        in
          Command.send (\() -> dummyCommand) initialState
            |> Command.expectDummy "fakeCommand"
            |> Expect.equal (Expect.fail (format [fact "No dummy commands sent with identifier" "fakeCommand"]))
    ]
  , describe "when dummy commands with the identifier are found"
    [ test "it passes" <|
      \() ->
        let
          initialState = Elmer.given App.defaultModel App.view App.update
          dummyCommand = Command.dummy "fakeCommand"
        in
          Command.send (\() -> dummyCommand) initialState
            |> Command.expectDummy "fakeCommand"
            |> Expect.equal Expect.pass
    ]
  ]
