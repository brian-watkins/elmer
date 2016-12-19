module Elmer.HttpTests exposing (all)

import Test exposing (..)
import Expect
import Http
import Dict
import Elmer
import Elmer.Types exposing (..)
import Elmer.Event as Event
import Elmer.Matchers as Matchers
import Elmer.Http as ElmerHttp
import Elmer.Http.Stub as HttpStub
import Elmer.Http.Matchers exposing (..)

import Elmer.TestApp as App

all : Test
all =
  describe "Http Tests"
  [ httpSendTests
  , requestRecordTests
  , errorResponseTests
  , expectPostTests
  , expectGetTests
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
          [ test "it fails with a message" <|
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
                  |> Elmer.expectNode (Matchers.hasText "Name: Super Fun Person")
                  |> Expect.equal (Expect.fail "Parsing a stubbed response\n\n\tGET http://fun.com/fun.html\n\n\t{}\n\nfailed with error\n\n\tExpecting an object with a field named `name` but instead got: {}\n\nIf you really want to generate a BadPayload error, consider using\nElmer.Http.Stub.withError to build your stubbed response.")
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

expectPostTests : Test
expectPostTests =
  describe "expectPost"
  [ describe "when there is an upstream error"
    [ test "it fails with the upstream error" <|
      \() ->
        ElmerHttp.expectPOST "http://fun.com/fun" hasAnyBody (UpstreamFailure "Failed!")
          |> Expect.equal (Expect.fail "Failed!")
    ]
  , describe "when no requests have been recorded"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          ElmerHttp.expectPOST "http://fun.com/fun" hasAnyBody initialState
            |> Expect.equal (Expect.fail "Expected request for\n\n\tPOST http://fun.com/fun\n\nbut no requests have been made")
    ]
  , describe "when requests have been recorded"
    [ describe "when the url does not match any requests"
      [ test "it fails" <|
        \() ->
          let
            defaultModel = App.defaultModel
            stubbedGet = HttpStub.get "http://awesome.com/awesome.html"
              |> HttpStub.withBody "{\"data\":\"Super Fun Person\"}"
            stubbedResponse = HttpStub.post "http://fun.com/fun"
              |> HttpStub.withBody "{\"name\":\"cool dude\"}"
            testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse)
              , anotherHttpSend = (ElmerHttp.fakeHttpSend stubbedGet)
              }
            initialState = Elmer.componentState testModel App.view App.update
          in
            Elmer.find "#another-request-data-click" initialState
              |> Event.click
              |> Elmer.find "#create-stuff-click"
              |> Event.click
              |> ElmerHttp.expectPOST "http://fun.com/awesome" hasAnyBody
              |> Expect.equal (Expect.fail "Expected request for\n\n\tPOST http://fun.com/awesome\n\nbut only found these requests\n\n\tPOST http://fun.com/fun\n\n\tGET http://awesome.com/awesome.html")

      ]
    , describe "when the url matches but not the method"
      [ test "it fails" <|
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
              |> ElmerHttp.expectPOST "http://fun.com/fun.html" hasAnyBody
              |> Expect.equal (Expect.fail "Expected request for\n\n\tPOST http://fun.com/fun.html\n\nbut only found these requests\n\n\tGET http://fun.com/fun.html")
      ]
    , describe "when a matching request occurs"
      [ describe "when the request expectations fail"
        [ test "it fails" <|
          \() ->
            let
              defaultModel = App.defaultModel
              stubbedResponse = HttpStub.post "http://fun.com/fun"
                |> HttpStub.withBody "{\"name\":\"cool dude\"}"
              testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse) }
              initialState = Elmer.componentState testModel App.view App.update
              failRequest = (\_ -> Expect.fail "It failed!")
            in
              Elmer.find "#create-stuff-click" initialState
                |> Event.click
                |> ElmerHttp.expectPOST "http://fun.com/fun" failRequest
                |> Expect.equal (Expect.fail "It failed!")
        ]
      , describe "when the request expectations pass"
        [ test "it passes" <|
          \() ->
            let
              defaultModel = App.defaultModel
              stubbedResponse = HttpStub.post "http://fun.com/fun"
                |> HttpStub.withBody "{\"name\":\"cool dude\"}"
              testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse) }
              initialState = Elmer.componentState testModel App.view App.update
            in
              Elmer.find "#create-stuff-click" initialState
                |> Event.click
                |> ElmerHttp.expectPOST "http://fun.com/fun" hasAnyBody
                |> Expect.equal (Expect.pass)
        ]
      ]
    ]
  ]

expectGetTests : Test
expectGetTests =
  describe "expectGET"
  [ describe "when there is an upstream error"
    [ test "it fails with the upstream error" <|
      \() ->
        ElmerHttp.expectGET "http://fun.com/fun" exists (UpstreamFailure "Failed!")
          |> Expect.equal (Expect.fail "Failed!")
    ]
  , describe "when no requests have been recorded"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          ElmerHttp.expectGET "http://fun.com/fun" exists initialState
            |> Expect.equal (Expect.fail "Expected request for\n\n\tGET http://fun.com/fun\n\nbut no requests have been made")
    ]
  , describe "when requests have been recorded"
    [ describe "when the url does not match any requests"
      [ test "it fails" <|
        \() ->
          let
            defaultModel = App.defaultModel
            stubbedGet = HttpStub.get "http://awesome.com/awesome.html"
              |> HttpStub.withBody "{\"data\":\"Super Fun Person\"}"
            stubbedResponse = HttpStub.post "http://fun.com/fun"
              |> HttpStub.withBody "{\"name\":\"cool dude\"}"
            testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse)
              , anotherHttpSend = (ElmerHttp.fakeHttpSend stubbedGet)
              }
            initialState = Elmer.componentState testModel App.view App.update
          in
            Elmer.find "#another-request-data-click" initialState
              |> Event.click
              |> Elmer.find "#create-stuff-click"
              |> Event.click
              |> ElmerHttp.expectGET "http://fun.com/awesome" exists
              |> Expect.equal (Expect.fail "Expected request for\n\n\tGET http://fun.com/awesome\n\nbut only found these requests\n\n\tPOST http://fun.com/fun\n\n\tGET http://awesome.com/awesome.html")
      ]
    , describe "when the url matches but not the method"
      [ test "it fails" <|
        \() ->
          let
            defaultModel = App.defaultModel
            stubbedResponse = HttpStub.post "http://fun.com/fun"
              |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
            testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse) }
            initialState = Elmer.componentState testModel App.view App.update
          in
            Elmer.find "#create-stuff-click" initialState
              |> Event.click
              |> ElmerHttp.expectGET "http://fun.com/fun" exists
              |> Expect.equal (Expect.fail "Expected request for\n\n\tGET http://fun.com/fun\n\nbut only found these requests\n\n\tPOST http://fun.com/fun")
      ]
    , describe "when a matching request occurs"
      [ describe "when the request expectations fail"
        [ test "it fails" <|
          \() ->
            let
              defaultModel = App.defaultModel
              stubbedResponse = HttpStub.get "http://fun.com/fun.html"
                |> HttpStub.withBody "{\"name\":\"cool dude\"}"
              testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse) }
              initialState = Elmer.componentState testModel App.view App.update
              failRequest = (\_ -> Expect.fail "It failed!")
            in
              Elmer.find "#request-data-click" initialState
                |> Event.click
                |> ElmerHttp.expectGET "http://fun.com/fun.html" failRequest
                |> Expect.equal (Expect.fail "It failed!")
        ]
      , describe "when the request expectations pass"
        [ test "it passes" <|
          \() ->
            let
              defaultModel = App.defaultModel
              stubbedResponse = HttpStub.get "http://fun.com/fun.html"
                |> HttpStub.withBody "{\"name\":\"cool dude\"}"
              testModel = { defaultModel | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse) }
              initialState = Elmer.componentState testModel App.view App.update
            in
              Elmer.find "#request-data-click" initialState
                |> Event.click
                |> ElmerHttp.expectGET "http://fun.com/fun.html" exists
                |> Expect.equal (Expect.pass)
        ]
      ]
    ]
  ]
