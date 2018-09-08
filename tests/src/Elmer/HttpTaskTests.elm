module Elmer.HttpTaskTests exposing (..)

import Test exposing (..)
import Expect
import Elmer exposing (exactly)
import Elmer.Html as Markup
import Elmer.Html.Event as Event
import Elmer.Html.Matchers exposing (element, hasText)
import Elmer.Html.Selector as Sel exposing (..)
import Elmer.Http
import Elmer.Http.Stub as HttpStub exposing (withBody, deferResponse, withStatus)
import Elmer.Http.Status as Status
import Elmer.Http.Route as Route exposing (get)
import Elmer.Http.Matchers exposing (hasQueryParam, wasRequested)
import Elmer.Spy as Spy
import Elmer.Platform.Command as Command
import Elmer.Printer exposing (..)

import Elmer.TestApps.HttpTestApp as App


all : Test
all =
  Test.concat
  [ httpServerTests
  , httpSpyTests
  , deferredResponseServerTests
  , andThenTaskTests
  ]


httpServerTests : Test
httpServerTests =
  describe "when Http.toTask is called while Elmer.Http.server is used"
  [ describe "when the request is stubbed" <|
    let
      stubbedResponse = HttpStub.for (get "http://fun.com/fun.html")
        |> withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
      otherStubbedResponse = HttpStub.for (get "http://fun.com/super.html")
        |> withBody "{\"name\":\"Super Fun Person\",\"message\":\"is cool\"}"
      state =
        Elmer.given App.defaultModel App.view App.update
          |> Spy.use [ Elmer.Http.serve [ stubbedResponse, otherStubbedResponse ] ]
          |> Markup.target << by [ id "request-data-with-task-click" ]
          |> Event.click
    in
    [ test "it processes the stubbed response" <|
      \() ->
        state
          |> Markup.target << by [ id "data-result" ]
          |> Markup.expect (element <| hasText "Data from Http Task: Super Fun Person is cool")
    , test "it records the request" <|
      \() ->
        state
          |> Elmer.Http.expect (get "http://fun.com/fun.html")
    ]
  , describe "when the request is not stubbed"
    [ test "it reports an error" <|
      \() ->
        let
          stubbedResponse = HttpStub.for (get "http://awesome.com/awesome.html")
            |> withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
        in
          Elmer.given App.defaultModel App.view App.update
            |> Spy.use [ Elmer.Http.serve [ stubbedResponse ] ]
            |> Markup.target << by [ id "request-data-with-task-click" ]
            |> Event.click
            |> Elmer.Http.expect (get "http://fun.com/fun.html")
            |> Expect.equal (Expect.fail (format
              [ message "Received a request for" "GET http://fun.com/fun.html"
              , message "but it does not match any of the stubbed requests" "GET http://awesome.com/awesome.html"
              ]
            ))
    ]
  ]

httpSpyTests : Test
httpSpyTests =
  describe "when Http.toTask is called while Elmer.Http.spy is used" <|
  let
    state =
      Elmer.given App.defaultModel App.view App.update
        |> Spy.use [ Elmer.Http.spy ]
        |> Markup.target << by [ id "request-data-with-task-click" ]
        |> Event.click
  in
  [ test "it does nothing in response to the request" <|
    \() ->
      state
        |> Markup.target << by [ id "data-result" ]
        |> Markup.expect (element <| hasText "")
  , test "it records the first request" <|
    \() ->
      state
        |> Elmer.Http.expect (get "http://fun.com/fun.html")
  , test "it does not record subsequent requests" <|
    \() ->
      state
        |> Elmer.Http.expect (get "http://fun.com/super.html")
        |> Expect.equal (Expect.fail (format
          [ message "Expected request for" "GET http://fun.com/super.html"
          , message "but only found these requests" "GET http://fun.com/fun.html"
          ]
        ))
  ]


deferredResponseServerTests : Test
deferredResponseServerTests =
  describe "when the HttpResponseStub is deferred while using Server" <|
  let
    stubbedResponse = HttpStub.for (get "http://fun.com/fun.html")
      |> withBody "{\"name\":\"Super Awesome Person\",\"type\":\"person\"}"
      |> deferResponse
    otherStubbedResponse = HttpStub.for (get "http://fun.com/super.html")
      |> withBody "{\"name\":\"Super Fun Person\",\"message\":\"is fun\"}"
      |> deferResponse
    state =
      Elmer.given App.defaultModel App.view App.update
        |> Spy.use [ Elmer.Http.serve [ stubbedResponse, otherStubbedResponse ] ]
        |> Markup.target << by [ id "request-data-with-task-click" ]
        |> Event.click
  in
  [ test "it does nothing" <|
    \() ->
      state
        |> Markup.target << by [ id "data-result" ]
        |> Markup.expect (element <| hasText "")
  , describe "when the deferred responses are resolved"
    [ test "it processes the stubbed response" <|
      \() ->
        state
          |> Command.resolveDeferred
          |> Markup.target << by [ id "data-result" ]
          |> Markup.expect (element <| hasText "Data from Http Task: Super Awesome Person is fun")
    , test "it records the request" <|
      \() ->
        state
          |> Command.resolveDeferred
          |> Elmer.Http.expect (get "http://fun.com/fun.html")
    ]
  ]

andThenTaskTests : Test
andThenTaskTests =
  describe "http task andThen"
  [ describe "when all the requests succeed" <|
    let
      stubbedResponse = HttpStub.for (get "http://fun.com/fun.html")
        |> withBody "{\"name\":\"Jerry\",\"type\":\"person\"}"
      otherStubbedResponse = HttpStub.for (get "http://fun.com/super.html")
        |> withBody "{\"name\":\"Super Fun Person\",\"message\":\"bowling\"}"
      state =
        Elmer.given App.defaultModel App.view App.update
          |> Spy.use [ Elmer.Http.serve [ stubbedResponse, otherStubbedResponse ] ]
          |> Markup.target << by [ id "request-other-data-with-task-click" ]
          |> Event.click
    in
    [ test "it processes the stubbed response" <|
      \() ->
        state
          |> Markup.target << by [ id "other-data-result" ]
          |> Markup.expect (element <| hasText "Data from Task: bowling")
    , test "it records the first request" <|
      \() ->
        state
          |> Elmer.Http.expect (get "http://fun.com/fun.html")
    , test "it records the second request" <|
      \() ->
        state
          |> Elmer.Http.expectThat (get "http://fun.com/super.html") (
            exactly 1 <| hasQueryParam ("name", "Jerry")
          )
    ]
  , describe "when the first request fails" <|
    let
      stubbedResponse = HttpStub.for (get "http://fun.com/fun.html")
        |> withStatus Status.serverError
      otherStubbedResponse = HttpStub.for (get "http://fun.com/super.html")
        |> withBody "{\"name\":\"Super Fun Person\",\"message\":\"bowling\"}"
      state =
        Elmer.given App.defaultModel App.view App.update
          |> Spy.use [ Elmer.Http.serve [ stubbedResponse, otherStubbedResponse ] ]
          |> Markup.target << by [ id "request-other-data-with-task-click" ]
          |> Event.click
    in
    [ test "it processes the stubbed response" <|
      \() ->
        state
          |> Markup.target << by [ id "other-data-result" ]
          |> Markup.expect (element <| hasText "Error!")
    , test "it records the first request" <|
      \() ->
        state
          |> Elmer.Http.expect (get "http://fun.com/fun.html")
    , test "it does not record the second request" <|
      \() ->
        state
          |> Elmer.Http.expectThat (get "http://fun.com/super.html") (
            wasRequested 0
          )
    ]
  , describe "when the second request fails" <|
    let
      stubbedResponse = HttpStub.for (get "http://fun.com/fun.html")
        |> withBody "{\"name\":\"Jerry\",\"type\":\"person\"}"
      otherStubbedResponse = HttpStub.for (get "http://fun.com/super.html")
        |> withStatus Status.serverError
      state =
        Elmer.given App.defaultModel App.view App.update
          |> Spy.use [ Elmer.Http.serve [ stubbedResponse, otherStubbedResponse ] ]
          |> Markup.target << by [ id "request-other-data-with-task-click" ]
          |> Event.click
    in
    [ test "it processes the stubbed response" <|
      \() ->
        state
          |> Markup.target << by [ id "other-data-result" ]
          |> Markup.expect (element <| hasText "Error!")
    , test "it records the first request" <|
      \() ->
        state
          |> Elmer.Http.expect (get "http://fun.com/fun.html")
    , test "it records the second request" <|
      \() ->
        state
          |> Elmer.Http.expectThat (get "http://fun.com/super.html") (
            exactly 1 <| hasQueryParam ("name", "Jerry")
          )
    ]
  ]
