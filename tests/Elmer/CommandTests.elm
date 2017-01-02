module Elmer.CommandTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Command
import Elmer
import Elmer.Types exposing (..)
import Elmer.TestApps.SimpleTestApp as App
import Elmer.TestApps.MessageTestApp as MessageApp
import Elmer.Event as Event
import Elmer.Matchers as Matchers

all : Test
all =
  describe "Command Tests"
  [ elmerFailureCommandTest
  , elmerMessageCommandTest
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
