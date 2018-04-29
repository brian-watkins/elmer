module Elmer.HttpStubTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.Http as ElmerHttp
import Elmer.Http.Internal as HttpInternal exposing (..)
import Elmer.Http.Stub as HttpStub
import Elmer.Http.Status as Status
import Elmer.Http.Result as HttpResult
import Elmer.Http.Route as Route
import Http
import Dict


testRoute : String -> String -> HttpRoute
testRoute method url =
  { method = method
  , url = url
  }

forTests : Test
forTests =
  let
    (HttpResponseStub responseStub) = HttpStub.for <| testRoute "FAKE-METHOD" "FAKE-URL"
  in
    describe "for"
    [ test ("it creates a stubbed response with the right method") <|
      \() ->
        Expect.equal "FAKE-METHOD" responseStub.method
    , test "it has a status of 200 Ok" <|
      \() ->
        case responseStub.resultBuilder testRequest of
          Response response ->
            Expect.equal { code = 200, message = "Ok" } response.status
          Error _ ->
            Expect.fail "Should be a response"
    , test "it has the right url" <|
      \() ->
        case responseStub.resultBuilder testRequest of
          Response response ->
            Expect.equal "FAKE-URL" response.url
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
          case updatedResponse.resultBuilder testRequest of
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
            case updatedResponse.resultBuilder testRequest of
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
            case updatedResponse.resultBuilder testRequest of
              Response response ->
                Expect.equal { code = 404, message = "Not Found" } response.status
              Error _ ->
                Expect.fail "Should have a response"
      ]
    ]
  , describe "withHeader"
    [ testDoesNotUpdateResponseStatus (HttpStub.withHeader ("x-header-1", "x-value-1"))
    , describe "when the stubbed response has a response" <|
      let
        (HttpResponseStub updatedResponse) = 
          defaultResponseStub 
            |> HttpStub.withHeader ("x-header-1", "x-value-1")
            |> HttpStub.withHeader ("x-header-2", "x-value-2")
      in
      [ test "it adds the header to the response" <|
        \() ->
            case updatedResponse.resultBuilder testRequest of
              Response response ->
                Expect.equal (Just "x-value-1") (Dict.get "x-header-1" response.headers)
              Error _ ->
                Expect.fail "Should have a response"
      , test "it adds the other header to the response" <|
        \() ->
            case updatedResponse.resultBuilder testRequest of
              Response response ->
                Expect.equal (Just "x-value-2") (Dict.get "x-header-2" response.headers)
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

resultBuilderTests : Test
resultBuilderTests =
  describe "withResult"
  [ test "it builds upon the given result" <|
    \() ->
      let
        (HttpResponseStub updatedResponse) = defaultResponseStub
          |> HttpStub.withResult (\_ result ->
              HttpResult.withBody "fake-body" result
            )
      in
        case updatedResponse.resultBuilder testRequest of
          Response response ->
            Expect.equal response.url "http://fake.com"
          Error _ ->
            Expect.fail "Should be a response!"
  , test "it uses the result returned by the function" <|
    \() ->
      let
        (HttpResponseStub updatedResponse) = defaultResponseStub
          |> HttpStub.withResult (\_ result ->
              HttpResult.withBody "fake-body" result
            )
      in
        case updatedResponse.resultBuilder testRequest of
          Response response ->
            Expect.equal response.body "fake-body"
          Error _ ->
            Expect.fail "Should be a response!"
  ]

testDoesNotUpdateResponseStatus : (HttpInternal.HttpResponseStub -> HttpInternal.HttpResponseStub) -> Test
testDoesNotUpdateResponseStatus builder =
  describe "when the stubbed response is set to return an error"
    [ test "it does not update the response status" <|
      \() ->
        let
          (HttpResponseStub updatedResponse) = defaultErrorResponseStub |> builder
        in
          Expect.equal (Error Http.Timeout) <| updatedResponse.resultBuilder testRequest
    ]

testRequest : HttpRequest
testRequest =
  { method = "GET"
  , url = "http://fake.com"
  , headers = []
  , body = Nothing
  }

defaultResult : HttpResult
defaultResult =
  Response
    { url = "http://fake.com"
    , status = { code = 200, message = "Ok" }
    , headers = Dict.empty
    , body = ""
    }

defaultResponseStub : HttpResponseStub
defaultResponseStub =
  HttpResponseStub
    { url = "http://fake.com"
    , method = "GET"
    , resultBuilder = (\_ -> defaultResult)
    , deferResponse = False
    }

defaultErrorResponseStub : HttpResponseStub
defaultErrorResponseStub =
  HttpResponseStub
    { url = "http://fake.com"
    , method = "GET"
    , resultBuilder = (\_ -> Error Http.Timeout)
    , deferResponse = False
    }
