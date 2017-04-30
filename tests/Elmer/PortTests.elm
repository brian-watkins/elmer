module Elmer.PortTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.TestApps.PortTestApp as App
import Elmer.Spy as Spy exposing (andCallFake)
import Elmer.Spy.Matchers exposing (wasCalled)
import Elmer.Platform.Subscription as Subscription
import Elmer
import Elmer.Html as Markup
import Elmer.Html.Event as Event
import Elmer.Html.Matchers exposing (..)

all : Test
all =
  describe "Port Tests"
  [ portCommandTests
  , portSubscriptionTests
  ]

portCommandTests : Test
portCommandTests =
  describe "port command spy"
  [ test "it calls the spy associated with the port command" <|
    \() ->
      let
        spy = Spy.create "port-spy" (\_ -> App.sendJsData)
          |> andCallFake (\_ -> Cmd.none)
      in
        Elmer.componentState App.defaultModel App.view App.update
          |> Spy.use [ spy ]
          |> Markup.target "#send-port-command-button"
          |> Event.click
          |> Spy.expect "port-spy" (wasCalled 1)
  ]

portSubscriptionTests : Test
portSubscriptionTests =
  describe "port subscription spy"
  [ test "it uses the subscription spy to send messages" <|
    \() ->
      let
        spy = Spy.create "port-spy" (\_ -> App.receiveJsData)
          |> andCallFake (\tagger -> Subscription.fake "fakeReceive" tagger)
      in
        Elmer.componentState App.defaultModel App.view App.update
          |> Spy.use [ spy ]
          |> Subscription.with (\_ -> App.subscriptions)
          |> Subscription.send "fakeReceive" "some fake data"
          |> Markup.target "#js-data"
          |> Markup.expect (element <| hasText "some fake data")
  ]
