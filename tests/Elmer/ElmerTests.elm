module Elmer.ElmerTests exposing (all)

import Test exposing (..)
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Elmer.TestApps.InitTestApp as InitApp
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Internal as Internal exposing (..)
import Elmer.Html
import Elmer.Html.Matchers as Matchers
import Elmer.Platform.Command as Command
import Elmer.Http
import Elmer.Http.Matchers as HttpMatchers
import Task

all : Test
all =
  describe "Elmer Tests"
    [ mapToExpectationTests
    , initTests
    ]

mapToExpectationTests =
  describe "mapToExpectaion"
  [ describe "when there is an upstream error"
    [ test "it fails with the upstream error" <|
      \() ->
        Internal.mapToExpectation (\_ -> Expect.pass) (Failed "Failed!")
          |> Expect.equal (Expect.fail "Failed!")
    ]
  , describe "when there is no upstream failure"
    [ describe "when the mapper fails"
      [ test "it fails" <|
        \() ->
          let
            initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          in
            Internal.mapToExpectation (\_ -> Expect.fail "I failed!") initialState
              |> Expect.equal (Expect.fail "I failed!")
      ]
    , describe "when the mapper passes"
      [ test "it passes" <|
        \() ->
          let
            initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          in
            Internal.mapToExpectation (
              \componentState ->
                Expect.equal Nothing componentState.targetSelector
            ) initialState
              |> Expect.equal (Expect.pass)
      ]
    ]
  ]

initTests : Test
initTests =
  describe "init"
  [ describe "when there is a faiure"
    [ test "it fails" <|
      \() ->
        let
          initialState = Failed "You failed!"
        in
          Elmer.init ( InitApp.defaultModel "", Cmd.none ) initialState
            |> Expect.equal (Failed "You failed!")
    ]
  , let
      state = Elmer.componentState (InitApp.defaultModel "") InitApp.view InitApp.update
        |> Command.use [ Elmer.Http.spy ] (\s ->
          let
            initPair = InitApp.init { baseUrl = "http://fun.com/api" }
          in
            Elmer.init initPair s
        )
    in
      describe "when there is no failure"
      [ test "it sets the model" <|
        \() ->
          state
            |> Elmer.Html.find "#base-url"
            |> Elmer.Html.expectElement (Matchers.hasText "http://fun.com/api")
      , test "it sends the command" <|
        \() ->
          state
            |> Elmer.Http.expectGET "http://fun.com/api/token" HttpMatchers.hasBeenRequested
      ]
  , describe "when the command fails"
    [ test "it fails" <|
      \() ->
        let
          state = Elmer.componentState (InitApp.defaultModel "") InitApp.view InitApp.update
            |> Elmer.init ( InitApp.defaultModel "", Task.perform InitApp.Tag (Task.succeed "Yo") )
        in
          case state of
            Ready _ ->
              Expect.fail "Should have failed!"
            Failed message ->
              Expect.equal True <|
                String.contains "Elmer encountered a command it does not know how to run" message
    ]
  ]
