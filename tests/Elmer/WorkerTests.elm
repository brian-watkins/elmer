module Elmer.WorkerTests exposing (..)

import Test exposing (..)
import Expect
import Elmer
import Elmer.Platform.Subscription as Subscription
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Headless as Headless
import Elmer.TestApps.WorkerTestApp as App

fakeSub : Spy
fakeSub =
  Spy.create "fake-sub" (\_ -> App.incomingData)
    |> andCallFake (\tagger ->
      Subscription.fake "fake-sub" tagger
    )

workerTests : Test
workerTests =
  describe "given"
  [ test "it create a TestState for the worker" <|
    \() ->
      Headless.given App.initialModel App.update
        |> Spy.use [ fakeSub ]
        |> Subscription.with (\() -> App.subscriptions)
        |> Subscription.send "fake-sub" "Yo"
        |> Elmer.expectModel (\model ->
          Expect.equal model.data "Yo"
        )
  ]
