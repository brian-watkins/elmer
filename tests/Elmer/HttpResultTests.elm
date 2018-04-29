module Elmer.HttpResultTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.Http.Internal exposing (..)
import Elmer.Http.Result as HttpResult
import Http
import Dict


withBodyTests : Test
withBodyTests =
  describe "withBody"
  [ describe "when the result is an error"
    [ test "it returns the error" <|
      \() ->
        let
          result = Error Http.Timeout
        in
          HttpResult.withBody "Blah" result
            |> Expect.equal (Error Http.Timeout)
    ]
  , describe "when the result is a response"
    [ test "it updates the body" <|
      \() ->
        let
          result = HttpResult.withBody "Blah" responseResult
        in
          case result of
            Response response ->
              Expect.equal response.body "Blah"
            Error _ ->
              Expect.fail "Should be a response!"
    ]
  ]

withHeaderTests : Test
withHeaderTests =
  describe "withHeader"
  [ describe "when the result is an error"
    [ test "it returns the error" <|
      \() ->
        let
          result = Error Http.Timeout
        in
          HttpResult.withHeader ("x-header-1", "x-value-1") result
            |> Expect.equal (Error Http.Timeout)
    ]
  , describe "when the result is a response" <|
    let
      result =
        responseResult
          |> HttpResult.withHeader ("x-header-1", "x-value-1")
          |> HttpResult.withHeader ("x-header-2", "x-value-2")
    in
    [ test "it adds the header" <|
      \() ->
        case result of
          Response response ->
            Expect.equal (Just "x-value-1") (Dict.get "x-header-1" response.headers)
          Error _ ->
            Expect.fail "Should be a response!"
    , test "it adds the other header" <|
      \() ->
        case result of
          Response response ->
            Expect.equal (Just "x-value-2") (Dict.get "x-header-2" response.headers)
          Error _ ->
            Expect.fail "Should be a response!"
    ]
  ]

responseResult : HttpResult
responseResult =
  Response
    { url = "http://fake.com"
    , status = { code = 200, message = "Ok" }
    , headers = Dict.empty
    , body = ""
    }
