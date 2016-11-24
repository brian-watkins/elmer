module Elmer.HttpStubTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Http as ElmerHttp
import Elmer.Http.Stub as HttpStub
import Http
import Dict

all : Test
all =
  describe "http stub tests"
  [ requestBuilderTests
  , responseBuilderTests
  ]

requestBuilderTests : Test
requestBuilderTests =
  describe "request builders"
  [ describeRequestMethod "GET" HttpStub.get
  , describeRequestMethod "POST" HttpStub.post
  ]

describeRequestMethod : String -> (String -> ElmerHttp.HttpResponseStub) -> Test
describeRequestMethod method builder =
  describe method
    [ test ("it creates a " ++ method ++ " response") <|
      \() ->
        let
          responseStub = builder "http://fake.com"
        in
          Expect.equal method responseStub.method
    , test "it has a status of 200 Ok" <|
      \() ->
        let
          responseStub = builder "http://fake.com"
        in
          case responseStub.response of
            ElmerHttp.HttpResponse response ->
              Expect.equal { code = 200, message = "Ok" } response.status
            ElmerHttp.HttpError _ ->
              Expect.fail "Should be a response"
    , test "it has the right url" <|
      \() ->
        let
          responseStub = builder "http://fake.com"
        in
          case responseStub.response of
            ElmerHttp.HttpResponse response ->
              Expect.equal "http://fake.com" response.url
            ElmerHttp.HttpError _ ->
              Expect.fail "Should be a response"
    ]

responseBuilderTests : Test
responseBuilderTests =
  describe "response builders"
  [ describe "withError" <|
    [ test "it sets the stub to return an error response" <|
      \() ->
        let
          updatedResponse = defaultResponseStub |> HttpStub.withError Http.Timeout
        in
          case updatedResponse.response of
            ElmerHttp.HttpResponse _ ->
              Expect.fail "Should be an error response"
            ElmerHttp.HttpError error ->
              Expect.equal Http.Timeout error
    ]
  , describe "withBody"
    [ testDoesNotUpdateResponseStatus (HttpStub.withBody "{}")
    , describe "when the stubbed response has a response"
      [ test "it updates the body" <|
        \() ->
          let
            body = "{\"name\":\"fun\"}"
            updatedResponse = defaultResponseStub |> HttpStub.withBody body
          in
            case updatedResponse.response of
              ElmerHttp.HttpResponse response ->
                Expect.equal body response.body
              ElmerHttp.HttpError _ ->
                Expect.fail "Should have a response"
      ]
    ]
  , describe "withStatus"
    [ testDoesNotUpdateResponseStatus (HttpStub.withStatus (HttpStub.httpStatus 200 "Ok"))
    , describe "when the stubbed response has a response"
      [ test "it updates the response status" <|
        \() ->
          let
            updatedResponse = defaultResponseStub |> HttpStub.withStatus (HttpStub.httpStatus 404 "Not Found")
          in
            case updatedResponse.response of
              ElmerHttp.HttpResponse response ->
                Expect.equal { code = 404, message = "Not Found" } response.status
              ElmerHttp.HttpError _ ->
                Expect.fail "Should have a response"
      ]
    ]
  ]

testDoesNotUpdateResponseStatus : (ElmerHttp.HttpResponseStub -> ElmerHttp.HttpResponseStub) -> Test
testDoesNotUpdateResponseStatus builder =
  describe "when the stubbed response is set to return an error"
    [ test "it does not update the response status" <|
      \() ->
        let
          updatedResponse = defaultErrorResponseStub |> builder
        in
          Expect.equal defaultErrorResponseStub updatedResponse
    ]

defaultResponseStub : ElmerHttp.HttpResponseStub
defaultResponseStub =
  { url = "http://fake.com"
  , method = "GET"
  , response = ElmerHttp.HttpResponse
    { url = "http://fake.com"
    , status = { code = 200, message = "Ok" }
    , headers = Dict.empty
    , body = ""
    }
  }

defaultErrorResponseStub : ElmerHttp.HttpResponseStub
defaultErrorResponseStub =
  { url = "http://fake.com"
  , method = "GET"
  , response = ElmerHttp.HttpError Http.Timeout
  }
