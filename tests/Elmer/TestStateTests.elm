module Elmer.TestStateTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.TestState as TestState
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Elmer


mapToExpectationTests =
  describe "mapToExpectaion"
  [ describe "when there is an upstream error"
    [ test "it fails with the upstream error" <|
      \() ->
        TestState.mapToExpectation (\_ -> Expect.pass) (TestState.failure "Failed!")
          |> Expect.equal (Expect.fail "Failed!")
    ]
  , describe "when there is no upstream failure"
    [ describe "when the mapper fails"
      [ test "it fails" <|
        \() ->
          let
            initialState = Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          in
            TestState.mapToExpectation (\_ -> Expect.fail "I failed!") initialState
              |> Expect.equal (Expect.fail "I failed!")
      ]
    , describe "when the mapper passes"
      [ test "it passes" <|
        \() ->
          Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
            |> TestState.mapToExpectation (\context ->
                Expect.pass
              )
            |> Expect.equal (Expect.pass)
      ]
    ]
  ]
