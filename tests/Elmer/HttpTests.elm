module Elmer.HttpTests exposing (all)

import Test exposing (..)
import Expect
import Http
import Dict
import Elmer
import Html
import Elmer.Types exposing (..)
import Elmer.Event as Event
import Elmer.Matchers as Matchers
import Elmer.Http as ElmerHttp
import Elmer.Http.Stub as HttpStub
import Elmer.Http.Matchers exposing (..)
import Elmer.Printer exposing (..)

import Elmer.TestApps.HttpTestApp as App

all : Test
all =
  describe "Http Tests"
  [ httpSendTests
  , requestRecordTests
  , errorResponseTests
  , expectRequestTests "GET" ElmerHttp.expectGET
  , expectRequestTests "POST" ElmerHttp.expectPOST
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
          stubbedResponse = HttpStub.get "http://wrongUrl.com"
            |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
          fakeHttpSend = ElmerHttp.fakeHttpSend stubbedResponse
          initialState = Elmer.componentState App.defaultModel App.view (App.update fakeHttpSend)
        in
          Elmer.find "#request-data-click" initialState
            |> Event.click
            |> Elmer.find "#data-result"
            |> Elmer.expectNode (Matchers.hasText "Name: Super Fun Person")
            |> Expect.equal (Expect.fail (format [message "Received a request for" "http://fun.com/fun.html", message "but it has not been stubbed. The stubbed request is" "http://wrongUrl.com"]))
    ]
  , describe "when the stubbed route contains a query string"
    [ test "it fails with a message" <|
      \() ->
        let
          stubbedResponse = HttpStub.get "http://wrongUrl.com?type=fun"
            |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
          fakeHttpSend = ElmerHttp.fakeHttpSend stubbedResponse
          initialState = Elmer.componentState App.defaultModel App.view (App.update fakeHttpSend)
        in
          Elmer.find "#request-data-click" initialState
            |> Event.click
            |> Elmer.find "#data-result"
            |> Elmer.expectNode (Matchers.hasText "Name: Super Fun Person")
            |> Expect.equal (Expect.fail (format [message "Sent a request where a stubbed route contains a query string" "http://wrongUrl.com?type=fun", description "Stubbed routes may not contain a query string"]))
    ]
  , describe "when the requested url matches the stubbed response"
    [ describe "when the method does not match"
      [ test "it fails with a message" <|
        \() ->
          let
            stubbedResponse = HttpStub.post "http://fun.com/fun.html"
              |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
            fakeHttpSend = ElmerHttp.fakeHttpSend stubbedResponse
            initialState = Elmer.componentState App.defaultModel App.view (App.update fakeHttpSend)
          in
            Elmer.find "#request-data-click" initialState
              |> Event.click
              |> Elmer.find "#data-result"
              |> Elmer.expectNode (Matchers.hasText "Name: Super Fun Person")
              |> Expect.equal (Expect.fail (format [message "A response has been stubbed for" "http://fun.com/fun.html", description "but it expects a POST not a GET"]))
      ]
    , describe "when the method matches"
      [ describe "when the response status is outside the 200 range"
        [ test "it sends a BadStatus message" <|
          \() ->
            let
              stubbedResponse = HttpStub.get "http://fun.com/fun.html"
                |> HttpStub.withStatus (HttpStub.httpStatus 404 "Not Found")
              fakeHttpSend = ElmerHttp.fakeHttpSend stubbedResponse
              initialState = Elmer.componentState App.defaultModel App.view (App.update fakeHttpSend)
            in
              Elmer.find "#request-data-click" initialState
                |> Event.click
                |> Elmer.find "#data-result"
                |> Elmer.expectNode (Matchers.hasText "BadStatus Error: 404 Not Found")
        ]
      , describe "when the response status is in the 200 range"
        [ describe "when the response body cannot be processed"
          [ test "it fails with a message" <|
            \() ->
              let
                stubbedResponse = HttpStub.get "http://fun.com/fun.html"
                  |> HttpStub.withBody "{}"
                fakeHttpSend = ElmerHttp.fakeHttpSend stubbedResponse
                initialState = Elmer.componentState App.defaultModel App.view (App.update fakeHttpSend)
              in
                Elmer.find "#request-data-click" initialState
                  |> Event.click
                  |> Elmer.find "#data-result"
                  |> Elmer.expectNode (Matchers.hasText "Name: Super Fun Person")
                  |> Expect.equal (Expect.fail (format
                    [ message "Parsing a stubbed response" "GET http://fun.com/fun.html"
                    , description ("\t{}")
                    , message "failed with error" "Expecting an object with a field named `name` but instead got: {}"
                    , description "If you really want to generate a BadPayload error, consider using\nElmer.Http.Stub.withError to build your stubbed response."
                    ]
                  ))
          ]
        , describe "when the requested url has a query string"
          [ test "it matches the stubbed path" <|
            \() ->
              let
                defaultModel = App.defaultModel
                stubbedResponse = HttpStub.get "http://fun.com/fun.html"
                  |> HttpStub.withBody "{\"name\":\"awesome things\"}"
                fakeHttpSend = ElmerHttp.fakeHttpSend stubbedResponse
                testModel = { defaultModel | query = "?type=awesome" }
                initialState = Elmer.componentState testModel App.view (App.update fakeHttpSend)
              in
                Elmer.find "#request-data-click" initialState
                  |> Event.click
                  |> Elmer.find "#data-result"
                  |> Elmer.expectNode (Matchers.hasText "awesome things")
                  |> Expect.equal Expect.pass
          ]
        , describe "when the response body can be processed"
          [ test "it decodes the response" <|
            \() ->
              let
                stubbedResponse = HttpStub.get "http://fun.com/fun.html"
                  |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
                fakeHttpSend = ElmerHttp.fakeHttpSend stubbedResponse
                initialState = Elmer.componentState App.defaultModel App.view (App.update fakeHttpSend)
              in
                Elmer.find "#request-data-click" initialState
                  |> Event.click
                  |> Elmer.find "#data-result"
                  |> Elmer.expectNode (Matchers.hasText "Super Fun Person")
                  |> Expect.equal Expect.pass
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
        stubbedResponse = HttpStub.get "http://fun.com/fun.html"
          |> HttpStub.withError Http.Timeout
        fakeHttpSend = ElmerHttp.fakeHttpSend stubbedResponse
        initialState = Elmer.componentState App.defaultModel App.view (App.update fakeHttpSend)
      in
        Elmer.find "#request-data-click" initialState
          |> Event.click
          |> Elmer.find "#data-result"
          |> Elmer.expectNode (Matchers.hasText "Timeout Error")
  ]

type alias TestModel =
  { name: String }

type TestMsg
  = TestMsg

componentStateWithRequests : List HttpRequestData -> ComponentStateResult TestModel TestMsg
componentStateWithRequests requestData =
  let
    state =
      { model = { name = "test" }
      , view = (\model -> Html.div [] [])
      , update = (\msg model -> (model, Cmd.none))
      , targetNode = Nothing
      , locationParser = Nothing
      , location = Nothing
      , httpRequests = requestData
      }
  in
    CurrentState state

testRequest : String -> String -> HttpRequestData
testRequest method url =
  { method = method
  , url = url
  , body = Nothing
  }

expectRequestTests : String -> (String -> (HttpRequestData -> Expect.Expectation) -> ComponentStateResult TestModel TestMsg -> Expect.Expectation) -> Test
expectRequestTests method func =
  describe ("expect" ++ method)
  [ describe "when there is an upstream error"
    [ test "it fails with the upstream error" <|
      \() ->
        func "http://fun.com/fun" hasBeenRequested (UpstreamFailure "Failed!")
          |> Expect.equal (Expect.fail "Failed!")
    ]
  , describe "when the expected route contains a query string"
    [ test "it fails with an error" <|
      \() ->
        let
          initialState = componentStateWithRequests []
        in
          func "http://fun.com/fun?type=amazing" hasBeenRequested initialState
            |> Expect.equal (Expect.fail (format [message "The expected route contains a query string" "http://fun.com/fun?type=amazing", description "Use the hasQueryParam matcher instead"]))
    ]
  , describe "when no requests have been recorded"
    [ test "it fails" <|
      \() ->
        let
          initialState = componentStateWithRequests []
        in
          func "http://fun.com/fun" hasBeenRequested initialState
            |> Expect.equal (Expect.fail (format [message "Expected request for" (method ++ " http://fun.com/fun"), description "but no requests have been made"]))
    ]
  , describe "when requests have been recorded"
    [ describe "when the url does not match any requests"
      [ test "it fails" <|
        \() ->
          let
            request1 = testRequest "POST" "http://fun.com/fun"
            request2 = testRequest "GET" "http://awesome.com/awesome.html?stuff=fun"
            initialState = componentStateWithRequests [ request1, request2 ]
          in
            func "http://fun.com/awesome" hasBeenRequested initialState
              |> Expect.equal (Expect.fail (format [message "Expected request for" (method ++ " http://fun.com/awesome"), message "but only found these requests" "POST http://fun.com/fun\n\n\tGET http://awesome.com/awesome.html?stuff=fun"]))
      ]
    , describe "when the url matches but not the method"
      [ test "it fails" <|
        \() ->
          let
            request1 = testRequest "OTHER_METHOD" "http://fun.com/fun"
            initialState = componentStateWithRequests [ request1 ]
          in
            func "http://fun.com/fun" hasBeenRequested initialState
              |> Expect.equal (Expect.fail (format [message "Expected request for" (method ++ " http://fun.com/fun"), message "but only found these requests" "OTHER_METHOD http://fun.com/fun"]))
      ]
    , describe "when a matching request occurs"
      [ describe "when the request expectations fail"
        [ test "it fails" <|
          \() ->
            let
              request1 = testRequest method "http://fun.com/fun.html"
              initialState = componentStateWithRequests [ request1 ]
              failRequest = (\_ -> Expect.fail "It failed!")
            in
              func "http://fun.com/fun.html" failRequest initialState
                |> Expect.equal (Expect.fail "It failed!")
        ]
      , describe "when the request expectations pass"
        [ test "it passes" <|
          \() ->
            let
              request1 = testRequest method "http://fun.com/fun.html"
              initialState = componentStateWithRequests [ request1 ]
            in
              func "http://fun.com/fun.html" hasBeenRequested initialState
                |> Expect.equal (Expect.pass)
        , describe "when the request has a query string"
          [ test "it passes" <|
            \() ->
              let
                request1 = testRequest method "http://awesome.com/awesome.html?stuff=fun"
                initialState = componentStateWithRequests [ request1 ]
              in
                func "http://awesome.com/awesome.html" (hasQueryParam ("stuff", "fun")) initialState
                  |> Expect.equal (Expect.pass)
          ]
        ]
      ]
    ]
  ]
