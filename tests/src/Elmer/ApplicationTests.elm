module Elmer.ApplicationTests exposing (all)

import Test exposing (..)
import Expect
import Elmer
import Elmer.TestState as TestState exposing (..)
import Elmer.Html as Markup
import Elmer.Html.Matchers exposing (element, hasText)
import Elmer.Html.Event as Event
import Elmer.Application
import Elmer.TestApps.ApplicationTestApp as App
import Elmer.Navigation as Navigation
import Elmer.Platform.Subscription as Subscription
import Elmer.Platform.Command as Command
import Elmer.Spy as Spy exposing (andCallFake)
import Elmer.Errors as Errors
import Url exposing (Url)


all : Test
all =
  Test.concat
  [ applicationTests
  , noInitTests
  ]


applicationTests : Test
applicationTests =
  describe "given an application"
  [ describe "when init is called"
    [ test "it creates a TestState" <|
      \() ->
        Elmer.Application.given App.OnUrlRequest App.OnUrlChange App.view App.update
          |> Elmer.init (\() -> App.init () (testUrl "http://localhost/app/fun") Navigation.fakeKey)
          |> Markup.target "#some-element"
          |> Markup.expect (element <| hasText "Fun Stuff")
    ]
  ]
  

noInitTests : Test
noInitTests =
  describe "when init is not called" 
  [ describe "when expecting an element"
    [ test "it shows an error" <|
      \() ->
        Elmer.Application.given App.OnUrlRequest App.OnUrlChange App.view App.update
          |> Markup.target "#some-element"
          |> Markup.expect (element <| hasText "Fun Stuff")
          |> Expect.equal (Expect.fail Errors.noModel)
    ]
  , describe "when simulating an event"
    [ test "it shows an error" <|
      \() ->
        Elmer.Application.given App.OnUrlRequest App.OnUrlChange App.view App.update
          |> Markup.target "#some-element"
          |> Event.click
          |> Markup.expect (element <| hasText "Fun Stuff")
          |> Expect.equal (Expect.fail Errors.noModel)
    ]
  , describe "when making an expectation about the model"
    [ test "it shows an error" <|
      \() ->
        Elmer.Application.given App.OnUrlRequest App.OnUrlChange App.view App.update
          |> Elmer.expectModel (\model -> Expect.fail "Should not get here")
          |> Expect.equal (Expect.fail Errors.noModel)
    ]
  , describe "when registering subscriptions"
    [ test "it shows an error" <|
      \() ->
        Elmer.Application.given App.OnUrlRequest App.OnUrlChange App.view App.update
          |> Subscription.with (\_ -> App.subscriptions)
          |> Elmer.expectModel (\model -> Expect.fail "Should not get here")
          |> Expect.equal (Expect.fail Errors.noModel)
    ]
  , describe "when processing a stubbed command"
    [ test "it shows an error" <|
      \() ->
        let
          funStub = 
            Spy.create "fun-spy" (\_ -> App.funCommand)
              |> andCallFake (\tagger message -> Command.fake <| tagger <| "FAKE: " ++ message)
        in
          Elmer.Application.given App.OnUrlRequest App.OnUrlChange App.view App.update
            |> Spy.use [ funStub ]
            |> Command.send (\_ -> App.funCommand App.FunTaskResult "hey!")
            |> Elmer.expectModel (\model -> Expect.fail "Should not get here")
            |> Expect.equal (Expect.fail Errors.noModel)
    ]
  ]
  

testUrl : String -> Url
testUrl urlString =
  case Url.fromString urlString of
    Just url ->
      url
    Nothing ->
      Debug.todo <| "Could not parse url: " ++ urlString