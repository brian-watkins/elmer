module Elmer.DemoAppTests exposing (..)

import Test exposing (..)
import Elmer.TestApps.DemoTestApp as App
import Elmer.TestApps.TimeTestApp as TimeApp
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Html.Event as Event
import Elmer.Html.Matchers as Matchers exposing (element, hasText)
import Elmer.Html.Selector as Sel exposing (by)
import Elmer.Navigation as ElmerNav
import Elmer.Http
import Elmer.Http.Stub as Stub
import Elmer.Http.Status as Status
import Elmer.Http.Route as Route
import Elmer.Spy as Spy exposing (Spy)
import Elmer.Command as Command
import Elmer.Html as Markup
import Elmer.Program
import Elmer.UrlHelpers as UrlHelpers

import Time exposing (Posix)
import Task exposing (Task)


all : Test
all =
  Test.concat
  [ appFlowTests
  , timeAppTests
  ]


successStub : String -> Elmer.Http.HttpResponseStub
successStub apiMessage =
  Stub.for (Route.get "/api/request")
    |> Stub.withBody apiMessage

failureStub : Elmer.Http.HttpResponseStub
failureStub =
  Stub.for (Route.get "/api/request")
    |> Stub.withStatus Status.serverError

appFlowTests =
  describe "app flow"
    [ test "it updates the model as events are processed and passes the expectation" <|
      \() ->
        Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.document App.update
          |> Spy.use [ ElmerNav.spy, Elmer.Http.serve [ (successStub "Ok" ) ] ]
          |> Elmer.Program.init (\_ -> App.init () (UrlHelpers.asUrl "http://local/click") ElmerNav.fakeKey)
          |> Markup.target << by [ Sel.class "button" ]
          |> Event.click
          |> Event.click
          |> Markup.target << by [ Sel.id "clickCount" ]
          |> Markup.expect (element <| hasText "2 clicks!")
          |> Expect.equal (Expect.pass)
    , test "it makes multiple expectations about a node" <|
      \() ->
        Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.document App.update
          |> Spy.use [ ElmerNav.spy ]
          |> Elmer.Program.init (\_ -> App.init () (UrlHelpers.asUrl "http://local/text") ElmerNav.fakeKey)
          |> Markup.target << by [ Sel.tag "ul" ]
          |> Markup.expect (element <| expectAll
              [ hasText "Fun Item 1"
              , hasText "Fun Item 2"
              , hasText "Fun Item 3"
              ]
            )
    , let
        resultState =
          Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.document App.update
            |> Spy.use [ ElmerNav.spy, Elmer.Http.serve [ (successStub "A message from the server!") ] ]
            |> Elmer.Program.init (\_ -> App.init () (UrlHelpers.asUrl "http://local/request") ElmerNav.fakeKey)
            |> Markup.target << by [ Sel.id "requestButton" ]
            |> Event.click
      in
        describe "successful http request"
        [ test "it displays the response body" <|
            \() ->
              resultState
                |> Markup.target << by [ Sel.id "requestOutput" ]
                |> Markup.expect (element <| hasText "Response: A message from the server!")
        , test "it does not display an error" <|
            \() ->
              resultState
                |> Markup.target << by [ Sel.id "requestError" ]
                |> Markup.expect (element <| hasText "Got request error: No error!")
        ]
    , let
        resultState =
          Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.document App.update
            |> Spy.use [ ElmerNav.spy, Elmer.Http.serve [ failureStub ] ]
            |> Elmer.Program.init (\_ -> App.init () (UrlHelpers.asUrl "http://local/request") ElmerNav.fakeKey)
            |> Markup.target << by [ Sel.id "requestButton" ]
            |> Event.click
      in
        describe "unsuccessful http request"
        [ test "it does not display a request output" <|
            \() ->
              resultState
                |> Markup.target << by [ Sel.id "requestOutput" ]
                |> Markup.expect (element <| hasText "Response: Error!")
        , test "it does display an error" <|
            \() ->
              resultState
                |> Markup.target << by [ Sel.id "requestError" ]
                |> Markup.expect (
                    element <| hasText "Got request error: Bad Status: 500 Internal Server Error"
                )
        ]
    ]

timeSpy : Int -> Spy
timeSpy time =
  Task.succeed (Time.millisToPosix time)
    |> Spy.replaceValue (\_ -> Time.now)

timeAppTests : Test
timeAppTests =
  describe "time demo app"
  [ test "it displays the time" <|
    \() ->
        Elmer.given TimeApp.defaultModel TimeApp.view TimeApp.update
          |> Markup.target << by [ Sel.class "button" ]
          |> Spy.use [ timeSpy 3000 ]
          |> Event.click
          |> Markup.target << by [ Sel.id "currentTime" ]
          |> Markup.expect (element <| hasText "Time: 3000")
  ]
