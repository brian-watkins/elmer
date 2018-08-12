module Elmer.HttpRequestTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.Http.Internal as HttpInternal
import Elmer.Http.Request as HttpRequest exposing (HttpRequest)
import Http
import Dict


all : Test
all =
  Test.concat
  [ bodyTests
  , queryStringTest
  , headersTest
  ]


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

headersTest : Test
headersTest =
  describe "headers"
  [ describe "when there are no headers"
    [ test "it returns an empty list" <|
      \() ->
        HttpRequest.headers (testRequest <| Nothing)
          |> Expect.equal []
    ]
  , describe "when there are headers"
    [ test "it returns a list of tuples" <|
      \() ->
        let
          headers =
            [ { name = "x-header-1", value = "x-value-1" }
            , { name = "x-header-2", value = "x-value-2" }
            ]
        in
          HttpRequest.headers (testHeadersRequest headers)
            |> Expect.equal [ ("x-header-1", "x-value-1"), ("x-header-2", "x-value-2") ]
    ]
  ]

testRequest : Maybe String -> HttpRequest
testRequest body =
  Http.request
    { method = "GET"
    , headers = []
    , url = "http://fake.com"
    , body = 
      case body of
        Just value ->
          Http.stringBody "application/json" value
        Nothing ->
          Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }
  |> HttpInternal.makeHttpRequest
  

testQueryRequest : String -> HttpRequest
testQueryRequest query =
  Http.request
    { method = "GET"
    , headers = []
    , url = "http://fake.com?" ++ query
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }
  |> HttpInternal.makeHttpRequest
  

testHeadersRequest : List { name : String, value : String } -> HttpRequest
testHeadersRequest headers =
  Http.request
    { method = "GET"
    , headers = List.map (\h -> Http.header h.name h.value) headers
    , url = "http://fake.com"
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }
  |> HttpInternal.makeHttpRequest
  