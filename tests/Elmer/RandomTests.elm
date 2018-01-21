module Elmer.RandomTests exposing (..)

import Test exposing (..)
import Expect
import Elmer exposing (exactly)
import Elmer.Headless as Headless
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Platform.Command as Command
import Random


type TestMsg
  = RandomInt Int


randomCommand : (Int -> TestMsg) -> Cmd TestMsg
randomCommand tagger =
  Random.int 0 10
    |> Random.andThen (\i -> Random.int i 10)
    |> Random.generate tagger


randomGenerateSpy : Spy
randomGenerateSpy =
  Spy.create "random-generate" (\_ -> Random.generate)
    |> andCallFake (\tagger generator ->
      Random.initialSeed 8000
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
      Headless.givenCommand (\_ -> randomCommand RandomInt)
        |> Spy.use [ randomGenerateSpy ]
        |> Headless.expectMessages (exactly 1 <|
          Expect.equal <| RandomInt 6
        )
  ]
