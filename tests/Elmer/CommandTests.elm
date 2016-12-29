module Elmer.CommandTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Command
import Elmer
import Elmer.Types exposing (..)
import Elmer.TestApps.SimpleTestApp as App
import Elmer.Event as Event

all : Test
all =
  describe "Command Tests"
  [ elmerFailureCommandTest
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
