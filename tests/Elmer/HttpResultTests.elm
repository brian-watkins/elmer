module Elmer.HttpResultTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Http.Internal exposing (..)
import Elmer.Http.Result as HttpResult
import Http
import Dict

all : Test
all =
  describe "HttpResult"
  [ withBodyTests
  ]

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

responseResult : HttpResult
responseResult =
  Response
    { url = "http://fake.com"
    , status = { code = 200, message = "Ok" }
    , headers = Dict.empty
    , body = ""
    }
