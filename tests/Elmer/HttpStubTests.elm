module Elmer.HttpStubTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Http as ElmerHttp
import Elmer.Http.Internal as HttpInternal exposing (..)
import Elmer.Http.Stub as HttpStub
import Elmer.Http.Status as Status
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
  , describeRequestMethod "DELETE" HttpStub.delete
  , describeRequestMethod "PUT" HttpStub.put
  , describeRequestMethod "PATCH" HttpStub.patch
  ]

describeRequestMethod : String -> (String -> HttpResponseStub) -> Test
describeRequestMethod method builder =
  describe method
    [ test ("it creates a " ++ method ++ " response") <|
      \() ->
        let
          (HttpResponseStub responseStub) = builder "http://fake.com"
        in
          Expect.equal method responseStub.method
    , test "it has a status of 200 Ok" <|
      \() ->
        let
          (HttpResponseStub responseStub) = builder "http://fake.com"
        in
          case responseStub.response of
            Response response ->
              Expect.equal { code = 200, message = "Ok" } response.status
            Error _ ->
              Expect.fail "Should be a response"
    , test "it has the right url" <|
      \() ->
        let
          (HttpResponseStub responseStub) = builder "http://fake.com"
        in
          case responseStub.response of
            Response response ->
              Expect.equal "http://fake.com" response.url
            Error _ ->
              Expect.fail "Should be a response"
    ]

responseBuilderTests : Test
responseBuilderTests =
  describe "response builders"
  [ describe "withError" <|
    [ test "it sets the stub to return an error response" <|
      \() ->
        let
          (HttpResponseStub updatedResponse) = defaultResponseStub |> HttpStub.withError Http.Timeout
        in
          case updatedResponse.response of
            Response _ ->
              Expect.fail "Should be an error response"
            Error error ->
              Expect.equal Http.Timeout error
    ]
  , describe "withBody"
    [ testDoesNotUpdateResponseStatus (HttpStub.withBody "{}")
    , describe "when the stubbed response has a response"
      [ test "it updates the body" <|
        \() ->
          let
            body = "{\"name\":\"fun\"}"
            (HttpResponseStub updatedResponse) = defaultResponseStub |> HttpStub.withBody body
          in
            case updatedResponse.response of
              Response response ->
                Expect.equal body response.body
              Error _ ->
                Expect.fail "Should have a response"
      ]
    ]
  , describe "withStatus"
    [ testDoesNotUpdateResponseStatus (HttpStub.withStatus Status.ok)
    , describe "when the stubbed response has a response"
      [ test "it updates the response status" <|
        \() ->
          let
            (HttpResponseStub updatedResponse) = defaultResponseStub |> HttpStub.withStatus Status.notFound
          in
            case updatedResponse.response of
              Response response ->
                Expect.equal { code = 404, message = "Not Found" } response.status
              Error _ ->
                Expect.fail "Should have a response"
      ]
    ]
  , describe "deferResponse"
    [ test "by default it does not defer the response" <|
      \() ->
        let
          (HttpResponseStub defaultStub) = defaultResponseStub
        in
          Expect.equal False defaultStub.deferResponse
    , test "it sets the response to be deferred" <|
      \() ->
        let
          (HttpResponseStub updatedResponse) = defaultResponseStub |> HttpStub.deferResponse
        in
          Expect.equal True updatedResponse.deferResponse
    ]
  ]

testDoesNotUpdateResponseStatus : (HttpInternal.HttpResponseStub -> HttpInternal.HttpResponseStub) -> Test
testDoesNotUpdateResponseStatus builder =
  describe "when the stubbed response is set to return an error"
    [ test "it does not update the response status" <|
      \() ->
        let
          updatedResponse = defaultErrorResponseStub |> builder
        in
          Expect.equal defaultErrorResponseStub updatedResponse
    ]

defaultResponseStub : HttpResponseStub
defaultResponseStub =
  HttpResponseStub
    { url = "http://fake.com"
    , method = "GET"
    , response = Response
      { url = "http://fake.com"
      , status = { code = 200, message = "Ok" }
      , headers = Dict.empty
      , body = ""
      }
    , deferResponse = False
    }

defaultErrorResponseStub : HttpResponseStub
defaultErrorResponseStub =
  HttpResponseStub
    { url = "http://fake.com"
    , method = "GET"
    , response = Error Http.Timeout
    , deferResponse = False
    }
