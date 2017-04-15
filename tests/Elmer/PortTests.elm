module Elmer.PortTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.TestApps.PortTestApp as App
import Elmer.Spy as Spy exposing (andCallFake)
import Elmer.Spy.Matchers exposing (wasCalled)
import Elmer
import Elmer.Html as Markup
import Elmer.Html.Event as Event

all : Test
all =
  describe "Port Tests"
  [ portCommandTests
  ]

portCommandTests : Test
portCommandTests =
  describe "port command spy"
  [ test "it calls the spy associated with the port command" <|
    \() ->
      let
        spy = Spy.create "port-spy" (\_ -> App.sendJsCommand)
          |> andCallFake (\_ -> Cmd.none)
      in
        Elmer.componentState App.defaultModel App.view App.update
          |> Spy.use [ spy ]
          |> Markup.find "#send-port-command-button"
          |> Event.click
          |> Spy.expect "port-spy" (wasCalled 1)
  ]
