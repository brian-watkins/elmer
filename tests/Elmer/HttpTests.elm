module Elmer.HttpTests exposing (all)

import Test exposing (..)
import Expect
import Http
import Dict
import Elmer
import Elmer.Event as Event
import Elmer.Matchers as Matchers
import Elmer.Http as ElmerHttp
import Elmer.Http.Stub as HttpStub

import Elmer.TestApp as App

all : Test
all =
  describe "Http Tests"
  [ httpSendTests
  , requestRecordTests
  , errorResponseTests
  ]

requestRecordTests : Test
requestRecordTests =
  let
    request = Http.request
      { method = "GET"
      , headers = [ Http.header "x-fun" "fun" ]
      , url = "http://myapp.com/fun.html"
      , body = Http.emptyBody
      , expect = Http.expectString
      , timeout = Nothing
      , withCredentials = False
      }
    httpRequest = ElmerHttp.asHttpRequest request
  in
    describe "RequestRecord"
    [ test "it has the method" <|
      \() ->
        Expect.equal httpRequest.method "GET"
    , test "it has the url" <|
      \() ->
        Expect.equal httpRequest.url "http://myapp.com/fun.html"
    ]

httpSendTests : Test
httpSendTests =
  describe "fakeHttpSend"
  [ describe "when the requested url is not stubbed"
    [ test "it fails with a message" <|
      \() ->
        let
          defaultModel = App.defaultModel
          stubbedResponse = HttpStub.get "http://wrongUrl.com"
            |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
          testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse) }
          initialState = Elmer.componentState testModel App.view App.update
        in
          Elmer.find "#request-data-click" initialState
            |> Event.click
            |> Elmer.find "#data-result"
            |> Elmer.expectNode (Matchers.hasText "Name: Super Fun Person")
            |> Expect.equal (Expect.fail "Received a request for\n\n\thttp://fun.com/fun.html\n\nbut it has not been stubbed. The stubbed request is\n\n\thttp://wrongUrl.com")
    ]
  , describe "when the requested url matches the stubbed response"
    [ describe "when the method does not match"
      [ test "it fails with a message" <|
        \() ->
          let
            defaultModel = App.defaultModel
            stubbedResponse = HttpStub.post "http://fun.com/fun.html"
              |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
            testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse) }
            initialState = Elmer.componentState testModel App.view App.update
          in
            Elmer.find "#request-data-click" initialState
              |> Event.click
              |> Elmer.find "#data-result"
              |> Elmer.expectNode (Matchers.hasText "Name: Super Fun Person")
              |> Expect.equal (Expect.fail "A response has been stubbed for\n\n\thttp://fun.com/fun.html\n\nbut it expects a POST not a GET")
      ]
    , describe "when the method matches"
      [ describe "when the response status is outside the 200 range"
        [ test "it sends a BadStatus message" <|
          \() ->
            let
              defaultModel = App.defaultModel
              stubbedResponse = HttpStub.get "http://fun.com/fun.html"
                |> HttpStub.withStatus (HttpStub.httpStatus 404 "Not Found")
              testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse) }
              initialState = Elmer.componentState testModel App.view App.update
            in
              Elmer.find "#request-data-click" initialState
                |> Event.click
                |> Elmer.find "#data-result"
                |> Elmer.expectNode (Matchers.hasText "BadStatus Error: 404 Not Found")
        ]
      , describe "when the response status is in the 200 range"
        [ describe "when the response body cannot be processed"
          [ test "it sends a BadPayload message" <|
            \() ->
              let
                defaultModel = App.defaultModel
                stubbedResponse = HttpStub.get "http://fun.com/fun.html"
                  |> HttpStub.withBody "{}"
                testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse) }
                initialState = Elmer.componentState testModel App.view App.update
              in
                Elmer.find "#request-data-click" initialState
                  |> Event.click
                  |> Elmer.find "#data-result"
                  |> Elmer.expectNode (Matchers.hasText "BadPayload Error: Expecting an object with a field named `name` but instead got: {}")
          ]
        , describe "when the response body can be processed"
          [ test "it decodes the response" <|
            \() ->
              let
                defaultModel = App.defaultModel
                stubbedResponse = HttpStub.get "http://fun.com/fun.html"
                  |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
                testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse) }
                initialState = Elmer.componentState testModel App.view App.update
              in
                Elmer.find "#request-data-click" initialState
                  |> Event.click
                  |> Elmer.find "#data-result"
                  |> Elmer.expectNode (Matchers.hasText "Name: Super Fun Person")
          ]
        ]
      ]
    ]
  ]

errorResponseTests : Test
errorResponseTests =
  describe "when the request should result in an Http.Error"
  [ test "it returns the error" <|
    \() ->
      let
        defaultModel = App.defaultModel
        stubbedResponse = HttpStub.get "http://fun.com/fun.html"
          |> HttpStub.withError Http.Timeout
        testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse) }
        initialState = Elmer.componentState testModel App.view App.update
      in
        Elmer.find "#request-data-click" initialState
          |> Event.click
          |> Elmer.find "#data-result"
          |> Elmer.expectNode (Matchers.hasText "Timeout Error")
  ]
