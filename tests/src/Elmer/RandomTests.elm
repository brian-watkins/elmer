module Elmer.RandomTests exposing (..)

import Test exposing (..)
import Expect
import Elmer exposing (exactly)
import Elmer.Command as Command
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Command as Command
import Random


all : Test
all =
  Test.concat
  [ randomTest
  ]


type TestMsg
  = RandomInt Int


randomCommand : (Int -> TestMsg) -> Cmd TestMsg
randomCommand tagger =
  Random.int 0 10
    |> Random.andThen (\i -> Random.int i 10)
    |> Random.generate tagger


randomGenerateSpy : Spy
randomGenerateSpy =
  Spy.observe (\_ -> Random.generate)
    |> andCallFake (\tagger generator ->
      Random.initialSeed 27852
        |> Random.step generator
        |> Tuple.first
        |> tagger
        |> Command.fake
    )


randomTest : Test
randomTest =
  describe "when a random value is requested"
  [ test "it returns a value provided by the generator" <|
    \() ->
      Command.given (\_ -> randomCommand RandomInt)
        |> Spy.use [ randomGenerateSpy ]
        |> Command.expectMessages (exactly 1 <|
          Expect.equal <| RandomInt 8
        )
  ]
