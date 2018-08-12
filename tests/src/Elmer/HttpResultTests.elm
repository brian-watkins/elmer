module Elmer.HttpResultTests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Elmer exposing (exactly)
import Elmer.Headless as Headless
import Elmer.Spy as Spy
import Elmer.Http.Result as HttpResult
import Elmer.Http.Status as Status
import Elmer.Http.Stub as Stub
import Elmer.Http.Route as Route
import Elmer.Http.Matchers as Matchers
import Elmer.Http exposing (HttpResponseStub)
import Http
import Dict exposing (Dict)
import Json.Decode as Json


all : Test
all =
  Test.concat
  [ withBodyTests
  , withHeaderTests
  , withStatusTests
  ]


defaultStub : HttpResponseStub
defaultStub =
  Stub.for (Route.get "http://fun.com")


type Msg
  = ResponseResult (Result Http.Error String)


expectMessageFor : HttpResponseStub -> Result Http.Error String -> Expectation
expectMessageFor stub result =
  let
    request = 
      Http.request 
        { method = "GET"
        , headers = []
        , url = "http://fun.com"
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
  in
    Headless.givenCommand (\() -> Http.send ResponseResult request)
      |> Spy.use [ Elmer.Http.serve [ stub ] ]
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (ResponseResult result)
      )


withBodyTests : Test
withBodyTests =
  describe "withBody"
  [ describe "when the result is an error"
    [ test "it returns the error" <|
      \() ->
        let
          stub = defaultStub 
            |> Stub.withError Http.Timeout
            |> Stub.withResult (\_ ->
              HttpResult.withBody "Blah"
            )
        in
          expectMessageFor stub (Err Http.Timeout)
    ]
  , describe "when the result is a response"
    [ test "it updates the body" <|
      \() ->
        let
          stub = defaultStub 
            |> Stub.withResult (\_ ->
              HttpResult.withBody "awesome"
            )
        in
          expectMessageFor stub (Ok "awesome")
    ]
  ]


expectHeadersFor : HttpResponseStub -> Dict String String -> Expectation
expectHeadersFor stub expectedHeaders =
  let
    request = 
      Http.request 
        { method = "GET"
        , headers = []
        , url = "http://fun.com"
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\response ->
            if response.headers == expectedHeaders then
              Ok "Found headers!"
            else
              Err <| "Wrong headers: " ++ Debug.toString response.headers
        )
        , timeout = Nothing
        , withCredentials = False
        }
  in
    Headless.givenCommand (\() -> Http.send ResponseResult request)
      |> Spy.use [ Elmer.Http.serve [ stub ] ]
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (ResponseResult <| Ok "Found headers!")
      )


withHeaderTests : Test
withHeaderTests =
  describe "withHeader"
  [ describe "when the result is an error"
    [ test "it returns the error" <|
      \() ->
        let
          stub = defaultStub 
            |> Stub.withError Http.Timeout
            |> Stub.withResult (\_ ->
              HttpResult.withHeader ("x-header-1", "x-value-1")
            )
        in
          expectMessageFor stub (Err Http.Timeout)
    ]
  , describe "when the result is a response" <|
    [ test "it adds the header" <|
      \() ->
        let
          stub = defaultStub
            |> Stub.withBody "\"cool\""
            |> Stub.withHeader ("x-header-1", "x-value-1")
            |> Stub.withResult (\_ ->
              HttpResult.withHeader ("x-header-2", "x-value-2")
            )
        in
          Dict.fromList [ ("x-header-1", "x-value-1"), ("x-header-2", "x-value-2") ]
            |> expectHeadersFor stub
    ]
  ]


expectStatusFor : HttpResponseStub -> Int -> Expectation
expectStatusFor stub expectedStatus =
  let
    request = 
      Http.request 
        { method = "GET"
        , headers = []
        , url = "http://fun.com"
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\response ->
            if response.status.code == expectedStatus then
              Ok "Found status!"
            else
              Err <| "Wrong status: " ++ String.fromInt response.status.code
        )
        , timeout = Nothing
        , withCredentials = False
        }
  in
    Headless.givenCommand (\() -> Http.send ResponseResult request)
      |> Spy.use [ Elmer.Http.serve [ stub ] ]
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (ResponseResult <| Ok "Found status!")
      )


withStatusTests : Test
withStatusTests =
  describe "withStatus"
  [ describe "when the result is an error"
    [ test "it returns the error" <|
      \() ->
        let
          stub = defaultStub 
            |> Stub.withError Http.Timeout
            |> Stub.withResult (\_ ->
              HttpResult.withStatus Status.ok
            )
        in
          expectMessageFor stub (Err Http.Timeout)    ]
  , describe "when the result is a response"
    [ test "it sets the status" <|
      \() ->
        let
          stub = defaultStub
            |> Stub.withBody "something"
            |> Stub.withResult (\_ ->
              HttpResult.withStatus Status.created
            )
        in
          expectStatusFor stub 201
    ]
  ]
