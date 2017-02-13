module Elmer.ElmerTests exposing (all)

import Test exposing (..)
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Internal as Internal exposing (..)

all : Test
all =
  describe "Elmer Tests"
    [ mapToExpectationTests
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
                Expect.equal Nothing componentState.targetElement
            ) initialState
              |> Expect.equal (Expect.pass)
      ]
    ]
  ]
