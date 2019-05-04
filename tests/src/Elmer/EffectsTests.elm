module Elmer.EffectsTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.Effects as Effects
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Elmer.TestState as TestState
import Elmer.Command as Command
import Elmer


all : Test
all =
  Test.concat
  [ expectEffectsTests
  , useTests
  ]


expectEffectsTests : Test
expectEffectsTests =
  describe "Effects.expect"
  [ describe "when there is an upstream error"
    [ test "it fails with the upstream error" <|
      \() ->
        Effects.expect FullState (\_ -> Expect.pass) (TestState.failure "Failed!")
          |> Expect.equal (Expect.fail "Failed!")
    ]
  , describe "when there is no upstream failure"
    [ describe "when the mapper fails"
      [ test "it fails" <|
        \() ->
          Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
            |> Command.send (\_ -> Effects.push FullState (\_ -> "Hello"))
            |> Effects.expect FullState (\_ -> Expect.fail "I failed!")
            |> Expect.equal (Expect.fail "I failed!")
      ]
    , describe "when the mapper passes"
      [ test "it passes" <|
        \() ->
          Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
            |> Command.send (\_ -> Effects.push FullState (\_ -> "Hello"))
            |> Effects.expect FullState (\_ -> Expect.pass)
            |> Expect.equal (Expect.pass)
      ]
    ]
    , describe "when the state is not set"
    [ test "it fails" <|
      \() ->
        Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
            |> Effects.expect EmptyState (Expect.equal <| Just "Something")
            |> Expect.equal (Expect.equal (Just "Something") Nothing)
    ]
    , describe "when the state is set"
    [ test "it passes the accumulated state to the matcher" <|
      \() ->
        Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
            |> Command.send (\_ -> Effects.push FullState (\_ -> "Hello"))
            |> Command.send (\_ -> Effects.push FullState (
                \word ->
                  Maybe.map (\m -> m ++ " Hello!") word
                    |> Maybe.withDefault ""
              ))
            |> Effects.expect FullState (Expect.equal <| Just "Hello Hello!")
            |> Expect.equal (Expect.pass)
    ]
  ]


useTests : Test
useTests =
  describe "Effects.use"
  [ describe "when there is an upstream failure"
    [ test "it passes the failure on without executing the step" <|
      \() ->
        TestState.failure "I've failed!"
          |> Effects.use FullState (\_ testState -> TestState.failure "yo")
          |> Effects.expect FullState (\_ -> Expect.pass)
          |> Expect.equal (Expect.fail "I've failed!")
    ]
  , describe "when there is no upstream failure"
    [ test "it executes the step and passes on the generated command" <|
      \() ->
        Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          |> Effects.use FullState (\_ testState -> testState |> Command.send (\_ -> Command.fail "I failed in a step"))
          |> Effects.expect FullState (\_ -> Expect.pass)
          |> Expect.equal (Expect.fail "I failed in a step")
    ]
    , describe "when the state is not set"
    [ test "it passes nothing to the step" <|
      \() ->
        Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
            |> Effects.use EmptyState (\value testState -> 
                case value of
                  Just _ ->
                    TestState.failure "Got something!"
                  Nothing ->
                    TestState.failure "Got nothing!"
              )
            |> Expect.equal (TestState.failure "Got nothing!")
    ]
    , describe "when the state is set"
    [ test "it passes the accumulated state to the step" <|
      \() ->
        Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
            |> Command.send (\_ -> Effects.push FullState (\_ -> "Hello"))
            |> Command.send (\_ -> Effects.push FullState (
                \word ->
                  Maybe.map (\m -> m ++ " Hello!") word
                    |> Maybe.withDefault ""
              ))
            |> Effects.use FullState (\value _ ->
                case value of
                  Just v ->
                    TestState.failure v
                  Nothing ->
                    TestState.failure "Got nothing!"
              )
            |> Expect.equal (TestState.failure "Hello Hello!")
    ]
  ]


type State
  = FullState
  | EmptyState