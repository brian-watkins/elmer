module Elmer.DemoAppTests exposing (..)

import Test exposing (..)
import Elmer.TestApps.DemoTestApp as App
import Elmer.TestApps.TimeTestApp as TimeApp
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Html.Event as Event
import Elmer.Html.Matchers as Matchers exposing (element, hasText)
import Elmer.Navigation as ElmerNav
import Elmer.Http
import Elmer.Http.Stub as Stub
import Elmer.Http.Status as Status
import Elmer.Http.Route as Route
import Elmer.Spy as Spy
import Elmer.Platform.Command as Command
import Elmer.Html as Markup

import Time exposing (Time)
import Task exposing (Task)


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
        Elmer.given App.defaultModel App.view App.update
          |> ElmerNav.withLocationParser App.urlParser
          |> Spy.use [ Elmer.Http.serve [ (successStub "Ok" ) ] ]
          |> ElmerNav.setLocation "/click"
          |> Markup.target ".button"
          |> Event.click
          |> Event.click
          |> Markup.target "#clickCount"
          |> Markup.expect (element <| hasText "2 clicks!")
          |> Expect.equal (Expect.pass)
    , test "it makes multiple expectations about a node" <|
      \() ->
        let
          initialState =
            Elmer.given App.defaultModel App.view App.update
              |> ElmerNav.withLocationParser App.urlParser
        in
          ElmerNav.setLocation "/text" initialState
            |> Markup.target "ul"
            |> Markup.expect (element <|
                hasText "Fun Item 1" <&&>
                hasText "Fun Item 2" <&&>
                hasText "Fun Item 3"
              )
    , let
        initialState =
          Elmer.given App.defaultModel App.view App.update
            |> ElmerNav.withLocationParser App.urlParser
        resultState = initialState
          |> Spy.use [ Elmer.Http.serve [ (successStub "A message from the server!") ] ]
          |> ElmerNav.setLocation "/request"
          |> Markup.target "#requestButton"
          |> Event.click
      in
        describe "successful http request"
        [ test "it displays the response body" <|
            \() ->
              Markup.target "#requestOutput" resultState
                |> Markup.expect (element <| hasText "Response: A message from the server!")
        , test "it does not display an error" <|
            \() ->
              Markup.target "#requestError" resultState
                |> Markup.expect (element <| hasText "Got request error: No error!")
        ]
    , let
        initialState =
          Elmer.given App.defaultModel App.view App.update
            |> ElmerNav.withLocationParser App.urlParser
        resultState = initialState
          |> Spy.use [ Elmer.Http.serve [ failureStub ] ]
          |> ElmerNav.setLocation "/request"
          |> Markup.target "#requestButton"
          |> Event.click
      in
        describe "unsuccessful http request"
        [ test "it does not display a request output" <|
            \() ->
              Markup.target "#requestOutput" resultState
                |> Markup.expect (element <| hasText "Response: Error!")
        , test "it does display an error" <|
            \() ->
              Markup.target "#requestError" resultState
                |> Markup.expect (
                    element <| hasText "Got request error: Bad Status: 500 Internal Server Error"
                )
        ]
    ]

fakeTaskPerform : Time -> (Time -> msg) -> Task Never Time -> Cmd msg
fakeTaskPerform time tagger task =
  Command.fake (tagger time)

timeAppTests : Test
timeAppTests =
  describe "time demo app"
  [ test "it displays the time" <|
    \() ->
      let
        initialState = Elmer.given TimeApp.defaultModel TimeApp.view TimeApp.update
        taskOverride = Spy.create "Task.perform" (\_ -> Task.perform)
          |> Spy.andCallFake (fakeTaskPerform (3 * Time.second))
      in
        Markup.target ".button" initialState
          |> Spy.use [ taskOverride ]
          |> Event.click
          |> Markup.target "#currentTime"
          |> Markup.expect (element <| hasText "Time: 3000")
  ]
