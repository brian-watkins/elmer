module Elmer.ElmerTests exposing (all)

import Test exposing (..)
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Types exposing (..)

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
        Elmer.mapToExpectation (\_ -> Expect.pass) (UpstreamFailure "Failed!")
          |> Expect.equal (Expect.fail "Failed!")
    ]
  , describe "when there is no upstream failure"
    [ describe "when the mapper fails"
      [ test "it fails" <|
        \() ->
          let
            initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          in
            Elmer.mapToExpectation (\_ -> Expect.fail "I failed!") initialState
              |> Expect.equal (Expect.fail "I failed!")
      ]
    , describe "when the mapper passes"
      [ test "it passes" <|
        \() ->
          let
            initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          in
            Elmer.mapToExpectation (
              \componentState ->
                Expect.equal Nothing componentState.targetNode
            ) initialState
              |> Expect.equal (Expect.pass)
      ]
    ]
  ]
