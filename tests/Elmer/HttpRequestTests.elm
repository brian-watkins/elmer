module Elmer.HttpRequestTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.Http.Internal exposing (..)
import Elmer.Http.Request as HttpRequest
import Http
import Dict


bodyTests : Test
bodyTests =
  describe "body"
  [ describe "when the body is Nothing"
    [ test "it returns Nothing" <|
      \() ->
        HttpRequest.body (testRequest Nothing)
          |> Expect.equal Nothing
    ]
  , describe "when the body is not Nothing"
    [ test "it returns the body" <|
      \() ->
        HttpRequest.body (testRequest <| Just "Something")
          |> Expect.equal (Just "Something")
    ]
  ]

queryStringTest : Test
queryStringTest =
  describe "queryString"
  [ describe "when there is no query string"
    [ test "it returns Nothing" <|
      \() ->
        HttpRequest.queryString (testRequest <| Just "Something")
          |> Expect.equal Nothing
    ]
  , describe "when there is a query string"
    [ test "it returns the query string" <|
      \() ->
        HttpRequest.queryString (testQueryRequest "fruit=apple&sport=bowling")
          |> Expect.equal (Just "fruit=apple&sport=bowling")
    ]
  ]

testRequest : Maybe String -> HttpRequest
testRequest body =
  { method = "GET"
  , url = "http://fake.com"
  , headers = []
  , body = body
  }

testQueryRequest : String -> HttpRequest
testQueryRequest query =
  { method = "GET"
  , url = "http://fake.com?" ++ query
  , headers = []
  , body = Nothing
  }
