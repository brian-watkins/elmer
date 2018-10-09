module Elmer.NavigationTests exposing (..)

import Test exposing (..)
import Elmer.TestApps.NavigationTestApp as App
import Expect
import Elmer exposing (..)
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Html.Event as Event
import Elmer.Spy as Spy
import Elmer.Platform.Command as Command
import Elmer.Navigation as ElmerNav
import Elmer.Program
import Elmer.Html.Matchers as Matchers exposing (..)
import Elmer.Html.Selector as Sel exposing (by)
import Elmer.Printer exposing (..)
import Elmer.Errors as Errors
import Elmer.Html as Markup
import Elmer.UrlHelpers as UrlHelpers
import Elmer.TestHelpers exposing (expectError)
import Browser exposing (UrlRequest)
import Url exposing (Url)


all : Test
all =
  Test.concat
  [ expectLocationTests
  ]


expectLocationTests =
  describe "expect location"
  [ describe "when there is an upstream failure"
    [ test "it reports the failure" <|
      \() ->
        TestState.failure "Nothing found!"
          |> ElmerNav.expectLocation "http://blah.com"
          |> Expect.equal (Expect.fail "Nothing found!")
    ]
  , describe "when a location has not been set"
    [ test "it explains the failure" <|
      \() ->
        Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.view App.update
          |> ElmerNav.expectLocation "http://badplace.com"
          |> expectError (Errors.noLocation "http://badplace.com")
    ]
  , describe "when a pushUrl command is sent"
    [ describe "when the correct url is expected"
      [ test "it passes" <|
          \() ->
            Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.view App.update
              |> Spy.use [ ElmerNav.spy ]
              |> Elmer.Program.init (\() -> App.init () (UrlHelpers.asUrl "http://localhost/fun.html") ElmerNav.fakeKey)
              |> Markup.target << by [ Sel.id "pushUrlButton" ]
              |> Event.click
              |> ElmerNav.expectLocation "http://fun.com/fun.html"
              |> Expect.equal Expect.pass
      ]
    , describe "when the incorrect url is expected"
      [ describe "when a location has been set"
        [ test "it explains the failure" <|
          \() ->
            Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.view App.update
              |> Spy.use [ ElmerNav.spy ]
              |> Elmer.Program.init (\() -> App.init () (UrlHelpers.asUrl "http://localhost/fun.html") ElmerNav.fakeKey)
              |> Markup.target << by [ Sel.id "pushUrlButton" ]
              |> Event.click
              |> ElmerNav.expectLocation "http://badplace.com"
              |> expectError (Errors.wrongLocation "http://badplace.com" "http://fun.com/fun.html")
        ]
      ]
    , describe "when the url cannot be parsed"
      [ test "it fails" <|
        \() ->
          Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.view App.update
            |> Spy.use [ ElmerNav.spy ]
            |> Elmer.Program.init (\() -> App.init () (UrlHelpers.asUrl "http://localhost/fun.html") ElmerNav.fakeKey)
            |> Markup.target << by [ Sel.id "pushBadUrl" ]
            |> Event.click
            |> ElmerNav.expectLocation "http://badplace.com"
            |> expectError (Errors.badUrl "Browser.Navigation.pushUrl" "kdshjfkdsjhfksd")
      ]
    , describe "when the test is not started as an application"
      [ test "it fails" <|
        \() ->
          Elmer.given App.defaultModel App.pageView App.update
            |> Spy.use [ ElmerNav.spy ]
            |> Markup.target << by [ Sel.id "pushUrlButton" ]
            |> Event.click
            |> ElmerNav.expectLocation "http://badplace.com"
            |> expectError (Errors.navigationSpyRequiresApplication "Browser.Navigation.pushUrl" "http://fun.com/fun.html")
      ]
    ]
  , describe "when a replaceUrl command is sent"
    [ describe "when the correct url is expected"
      [ test "it passes" <|
          \() ->
            Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.view App.update
              |> Spy.use [ ElmerNav.spy ]
              |> Elmer.Program.init (\() -> App.init () (UrlHelpers.asUrl "http://localhost/fun.html") ElmerNav.fakeKey)
              |> Markup.target << by [ Sel.id "replaceUrlButton" ]
              |> Event.click
              |> ElmerNav.expectLocation "http://fun.com/awesome.html"
              |> Expect.equal Expect.pass
      ]
    , describe "when the incorrect url is expected"
      [ describe "when a location has been set"
        [ test "it explains the failure" <|
          \() ->
            Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.view App.update
              |> Spy.use [ ElmerNav.spy ]
              |> Elmer.Program.init (\() -> App.init () (UrlHelpers.asUrl "http://localhost/fun.html") ElmerNav.fakeKey)
              |> Markup.target << by [ Sel.id "replaceUrlButton" ]
              |> Event.click
              |> ElmerNav.expectLocation "http://badplace.com"
              |> expectError (Errors.wrongLocation "http://badplace.com" "http://fun.com/awesome.html")
        ]
      ]
    , describe "when the url cannot be parsed"
      [ test "it fails" <|
        \() ->
          Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.view App.update
            |> Spy.use [ ElmerNav.spy ]
            |> Elmer.Program.init (\() -> App.init () (UrlHelpers.asUrl "http://localhost/fun.html") ElmerNav.fakeKey)
            |> Markup.target << by [ Sel.id "replaceBadUrl" ]
            |> Event.click
            |> ElmerNav.expectLocation "http://badplace.com"
            |> expectError (Errors.badUrl "Browser.Navigation.replaceUrl" "kdshjfkdsjhfksd")
      ]
    , describe "when the test is not started as an application"
      [ test "it fails" <|
        \() ->
          Elmer.given App.defaultModel App.pageView App.update
            |> Spy.use [ ElmerNav.spy ]
            |> Markup.target << by [ Sel.id "replaceUrlButton" ]
            |> Event.click
            |> ElmerNav.expectLocation "http://badplace.com"
            |> expectError (Errors.navigationSpyRequiresApplication "Browser.Navigation.replaceUrl" "http://fun.com/awesome.html")
      ]
    ]
  ]
