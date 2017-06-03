module Elmer.ComponentStateTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.ComponentState as ComponentState
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Elmer


mapToExpectationTests =
  describe "mapToExpectaion"
  [ describe "when there is an upstream error"
    [ test "it fails with the upstream error" <|
      \() ->
        ComponentState.mapToExpectation (\_ -> Expect.pass) (ComponentState.failure "Failed!")
          |> Expect.equal (Expect.fail "Failed!")
    ]
  , describe "when there is no upstream failure"
    [ describe "when the mapper fails"
      [ test "it fails" <|
        \() ->
          let
            initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          in
            ComponentState.mapToExpectation (\_ -> Expect.fail "I failed!") initialState
              |> Expect.equal (Expect.fail "I failed!")
      ]
    , describe "when the mapper passes"
      [ test "it passes" <|
        \() ->
          Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
            |> ComponentState.mapToExpectation (\component ->
                Expect.equal Nothing component.targetSelector
              )
            |> Expect.equal (Expect.pass)
      ]
    ]
  ]
