module Elmer.DemoAppTests exposing (all)

import Test exposing (..)
import Elmer.TestApps.DemoTestApp as App
import Elmer.TestApps.TimeTestApp as TimeApp
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Html.Event as Event
import Elmer.Html.Matchers as Matchers
import Elmer.Navigation as ElmerNav
import Elmer.Http
import Elmer.Http.Stub as Stub
import Elmer.Command as Command
import Elmer.Html as Markup

import Time exposing (Time)
import Task exposing (Task)

all : Test
all =
  describe "App Flow tests"
    [ appFlowTests
    , timeAppTests
    ]

testUpdate : Elmer.Http.HttpResponseStub -> (App.Msg -> App.Model -> (App.Model, Cmd App.Msg))
testUpdate stub =
  let
    fakeSend = Elmer.Http.fakeHttpSend stub
  in
    App.updateWithDependencies (App.requestComponentUpdateWithDependencies fakeSend)

successStub : String -> Elmer.Http.HttpResponseStub
successStub apiMessage =
  Stub.get "/api/request"
    |> Stub.withBody apiMessage

failureStub : Elmer.Http.HttpResponseStub
failureStub =
  Stub.get "/api/request"
    |> Stub.withStatus (Stub.httpStatus 500 "Internal Server Error")

appFlowTests =
  describe "app flow"
    [ test "it updates the model as events are processed and passes the expectation" <|
      \() ->
        let
          initialState = Elmer.navigationComponentState App.defaultModel App.view (testUpdate (successStub "Ok")) App.urlParser
        in
          ElmerNav.setLocation "/click" initialState
            |> Markup.find ".button"
            |> Event.click
            |> Event.click
            |> Markup.find "#clickCount"
            |> Markup.expectNode (Matchers.hasText "2 clicks!")
            |> Expect.equal (Expect.pass)
    , test "it makes multiple expectations about a node" <|
      \() ->
        let
          initialState = Elmer.navigationComponentState App.defaultModel App.view (testUpdate (successStub "Ok")) App.urlParser
        in
          ElmerNav.setLocation "/text" initialState
            |> Markup.find "ul"
            |> Markup.expectNode (
              Matchers.hasText "Fun Item 1"
              <&&> Matchers.hasText "Fun Item 2"
              <&&> Matchers.hasText "Fun Item 3"
              )
    , let
        initialState = Elmer.navigationComponentState App.defaultModel App.view (testUpdate (successStub "A message from the server!")) App.urlParser
        resultState = ElmerNav.setLocation "/request" initialState
          |> Markup.find "#requestButton"
          |> Event.click
      in
        describe "successful http request"
        [ test "it displays the response body" <|
            \() ->
              Markup.find "#requestOutput" resultState
                |> Markup.expectNode (Matchers.hasText "Response: A message from the server!")
        , test "it does not display an error" <|
            \() ->
              Markup.find "#requestError" resultState
                |> Markup.expectNode (Matchers.hasText "Got request error: No error!")
        ]
    , let
        initialState = Elmer.navigationComponentState App.defaultModel App.view (testUpdate failureStub) App.urlParser
        resultState = ElmerNav.setLocation "/request" initialState
          |> Markup.find "#requestButton"
          |> Event.click
      in
        describe "unsuccessful http request"
        [ test "it does not display a request output" <|
            \() ->
              Markup.find "#requestOutput" resultState
                |> Markup.expectNode (Matchers.hasText "Response: Error!")
        , test "it does display an error" <|
            \() ->
              Markup.find "#requestError" resultState
                |> Markup.expectNode (
                    Matchers.hasText "Got request error: Bad Status: 500 Internal Server Error"
                )
        ]
    ]

fakeTimeTask : Time -> (Time -> TimeApp.Msg) -> Task Never Time -> Cmd TimeApp.Msg
fakeTimeTask time tagger task =
  Command.stub (tagger time)

timeAppTests : Test
timeAppTests =
  describe "time demo app"
  [ test "it displays the time" <|
    \() ->
      let
        testUpdate = TimeApp.updateWithDependencies (fakeTimeTask (3 * Time.second))
        initialState = Elmer.componentState TimeApp.defaultModel TimeApp.view testUpdate
      in
        Markup.find ".button" initialState
          |> Event.click
          |> Markup.find "#currentTime"
          |> Markup.expectNode (Matchers.hasText "Time: 3000")
  ]
