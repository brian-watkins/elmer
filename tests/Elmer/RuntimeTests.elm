module Elmer.RuntimeTests exposing (all)

import Test exposing (..)
import Expect

import Elmer exposing (..)
import Elmer.Event as Event
import Elmer.TestApp as App
import Elmer.Runtime as Runtime
import Elmer.Matchers as Matchers
import Elmer.Http as ElmerHttp
import Elmer.Http.Stub as HttpStub

import Task

all : Test
all =
  describe "Runtime Tests"
  [ elmerFailureTaskTest
  , batchCommandTest
  , batchCommandFailureTest
  ]

elmerFailureTaskTest : Test
elmerFailureTaskTest =
  describe "when the Component Failure task is processed"
  [ test "it causes an upstream failure" <|
    \() ->
      let
        model = App.defaultModel
        initialState = Elmer.componentState model App.view App.update
        result = Event.sendCommand (Elmer.failureCommand "You failed!") initialState
      in
        Expect.equal (UpstreamFailure "You failed!") result
  ]





batchCommandTest : Test
batchCommandTest =
  let
    defaultModel = App.defaultModel
    stubbedResponse = HttpStub.get "http://fun.com/fun.html"
      |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
    anotherStub = HttpStub.get "http://awesome.com/awesome.html"
      |> HttpStub.withBody "{\"data\":\"some webservice data\"}"
    testModel =
      { defaultModel
      | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse)
      , anotherHttpSend = (ElmerHttp.fakeHttpSend anotherStub)
      }
    initialState = Elmer.componentState testModel App.view App.update
    batchCommand = Cmd.batch
      [ App.fetchData testModel
      , App.fetchMoreData testModel
      ]
    result = Event.sendCommand batchCommand initialState
  in
    describe "when a batch command is sent"
    [ test "it processes the first command" <|
      \() ->
          Elmer.find "#webservice-data" result
            |> Elmer.expectNode (Matchers.hasText "Name: Super Fun Person")
    , test "it processes the second command" <|
      \() ->
          Elmer.find "#another-webservice-data" result
            |> Elmer.expectNode (Matchers.hasText "some webservice data")
    ]

batchCommandFailureTest : Test
batchCommandFailureTest =
  let
    defaultModel = App.defaultModel
    stubbedResponse = HttpStub.get "http://fun.com/fun.html"
      |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
    anotherStub = HttpStub.get "http://awesome.com/awesome.html"
      |> HttpStub.withBody "{\"data\":\"some webservice data\"}"
    testModel =
      { defaultModel
      | httpSend = (ElmerHttp.fakeHttpSend stubbedResponse)
      , anotherHttpSend = (ElmerHttp.fakeHttpSend anotherStub)
      }
    initialState = Elmer.componentState testModel App.view App.update
    batchCommand = Cmd.batch
      [ App.fetchData testModel
      , Elmer.failureCommand "It failed!"
      , App.fetchMoreData testModel
      ]
    result = Event.sendCommand batchCommand initialState
  in
    describe "when a batched command fails"
    [ test "it reports the failure" <|
      \() ->
          Expect.equal (UpstreamFailure "It failed!") result
    ]
