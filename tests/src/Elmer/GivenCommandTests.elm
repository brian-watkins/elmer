module Elmer.GivenCommandTests exposing (..)

import Test exposing (..)
import Expect
import Elmer exposing (exactly)
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Spy.Matchers exposing (wasCalledWith, stringArg)
import Elmer.Command as Command
import Elmer.Message exposing (..)
import Task


all : Test
all =
  Test.concat
  [ spyTests
  , expectMessageTests
  ]


spyTests : Test
spyTests =
  describe "when the test for a given command uses a spy"
  [ test "it satisfies expectations about the spy" <|
    \() ->
      Command.given (\() -> spyCommand "hello")
        |> Spy.use [ testPortSpy ]
        |> Spy.expect (\_ -> testPortCommand) (
            wasCalledWith [ stringArg "hello" ]
        )
  ]


testPortSpy : Spy
testPortSpy =
  Spy.observe (\_ -> testPortCommand)
    |> andCallFake (\_ -> Cmd.none)


testPortCommand : String -> Cmd msg
testPortCommand _ =
  Cmd.none

spyCommand : String -> Cmd Msg
spyCommand message =
  testPortCommand message


type Msg
  = TestResult (Result String Int)


expectMessageTests : Test
expectMessageTests =
  describe "when a given command results in a message"
  [ describe "when the processing the command is successful"
    [ test "it records the messages" <|
      \() ->
        Command.given (\() -> Task.succeed 17 |> Task.attempt TestResult)
          |> Command.expectMessages (
            exactly 1 <| Expect.equal (TestResult <| Ok 17)
          )
    ]
  , describe "when processing the command results in a test failure"
    [ test "it records the messages" <|
      \() ->
        Command.given (\_ -> Command.fail "Failed!")
          |> Command.expectMessages (
            exactly 1 <| Expect.equal (TestResult <| Ok 17)
          )
          |> Expect.equal (Expect.fail "Failed!")
    ]
  ]
