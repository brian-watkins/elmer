module Elmer.RuntimeTests exposing (all)

import Test exposing (..)
import Expect

import Elmer exposing (..)
import Elmer.Event as Event
import Elmer.TestApp as App
import Elmer.Runtime as Runtime

import Task

all : Test
all =
  describe "Runtime Tests"
  [ elmerFailureTaskTest ]

elmerFailureTaskTest : Test
elmerFailureTaskTest =
  describe "when the Component Failure task is processed"
  [ test "it causes an upstream failure" <|
    \() ->
      let
        model = App.defaultModel
        initialState = Elmer.componentState model App.view App.update
        result = Event.sendCommand (Elmer.failureCommand "You failed!") initialState
      in
        Expect.equal (UpstreamFailure "You failed!") result
  ]
