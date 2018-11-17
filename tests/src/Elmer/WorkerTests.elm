module Elmer.WorkerTests exposing (..)

import Test exposing (..)
import Expect
import Elmer
import Elmer.Subscription as Subscription
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Program
import Elmer.TestApps.WorkerTestApp as App


all : Test
all =
  Test.concat
  [ workerTests
  ]


fakeSub : Spy
fakeSub =
  Spy.observe (\_ -> App.incomingData)
    |> andCallFake (\tagger ->
      Subscription.fake "fake-sub" tagger
    )

workerTests : Test
workerTests =
  describe "given"
  [ test "it create a TestState for the worker" <|
    \() ->
      Elmer.Program.givenWorker App.update
        |> Spy.use [ fakeSub ]
        |> Elmer.Program.init (\() -> (App.initialModel, Cmd.none))
        |> Subscription.with (\() -> App.subscriptions)
        |> Subscription.send "fake-sub" "Yo"
        |> Elmer.expectModel (\model ->
          Expect.equal model.data "Yo"
        )
  ]
