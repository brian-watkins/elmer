module Elmer.HttpRequestTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Http.Internal exposing (..)
import Elmer.Http.Request as HttpRequest
import Http
import Dict

all : Test
all =
  describe "HttpRequest"
  [ bodyTests
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

testRequest : Maybe String -> HttpRequest
testRequest body =
  { method = "GET"
  , url = "http://fake.com"
  , headers = []
  , body = body
  }
