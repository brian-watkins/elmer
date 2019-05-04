module Elmer.HttpMatcherTests exposing (..)

import Test exposing (..)
import Expect
import Http
import Elmer.Http.Internal as HttpInternal
import Elmer.Http.Request exposing (HttpRequest)
import Elmer.Http.Matchers as Matchers
import Elmer.Message exposing (..)


all : Test
all =
  Test.concat
  [ wasRequestedTests
  , hasBodyTests
  , hasQueryParamTests
  , hasHeaderTests
  ]


requestWithBody : String -> HttpRequest
requestWithBody body =
  Http.request
    { method = "POST"
    , headers = []
    , url = "http://fun.com/fun"
    , body = Http.stringBody "application/json" body
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }
  |> HttpInternal.makeHttpRequest

requestWithNoBody : HttpRequest
requestWithNoBody =
  Http.request
    { method = "POST"
    , headers = []
    , url = "http://fun.com/fun"
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }
  |> HttpInternal.makeHttpRequest


wasRequestedTests : Test
wasRequestedTests =
  describe "wasRequested"
  [ describe "when the list of requests is empty"
    [ test "it fails" <|
      \() ->
        Matchers.wasRequested 3 []
          |> Expect.equal (Expect.fail "Expected 3 requests, but recorded 0 requests")
    , describe "when the expected call count is 1"
      [ test "it prints a grammatic message" <|
        \() ->
          Matchers.wasRequested 1 []
            |> Expect.equal (Expect.fail "Expected 1 request, but recorded 0 requests")
      ]
    , describe "when the expected call count is 0"
      [ test "it passes" <|
        \() ->
          Matchers.wasRequested 0 []
            |> Expect.equal Expect.pass
      ]
    ]
  , describe "when the number of requests does not match the expected count"
    [ describe "when 1 request is recorded"
      [ test "it fails with a grammatical message" <|
        \() ->
          Matchers.wasRequested 3 [ requestWithBody "{}" ]
            |> Expect.equal (Expect.fail "Expected 3 requests, but recorded 1 request")
      ]
    , describe "when mutiple requests are recorded"
      [ test "it fails with a grammatical message" <|
        \() ->
          Matchers.wasRequested 3 [ requestWithBody "{}", requestWithBody "{}" ]
            |> Expect.equal (Expect.fail "Expected 3 requests, but recorded 2 requests")
      ]
    , describe "when the expected call count is 0"
      [ test "it prints a grammatical message" <|
        \() ->
          Matchers.wasRequested 0 [ requestWithBody "{}" ]
            |> Expect.equal (Expect.fail "Expected 0 requests, but recorded 1 request")
      ]
    , describe "when the expected call count is 1"
      [ test "it prints a grammatical message" <|
        \() ->
          Matchers.wasRequested 1 [ requestWithBody "{}", requestWithBody "{}" ]
            |> Expect.equal (Expect.fail "Expected 1 request, but recorded 2 requests")
      ]
    ]
  , describe "when the number of requests matches the expected count"
    [ test "it passes" <|
      \() ->
        Matchers.wasRequested 2 [ requestWithBody "{}", requestWithBody "{}" ]
          |> Expect.equal Expect.pass
    ]
  ]

hasBodyTests : Test
hasBodyTests =
  describe "hasBody"
  [ describe "when there is no body"
    [ test "it fails" <|
      \() ->
        Matchers.hasBody "{}" requestWithNoBody
          |> Expect.equal (Expect.fail (format [fact "Expected request to have body" "{}", note "but it has no body"]))
    ]
  , describe "when there is a body"
    [ describe "when the body does not match"
      [ test "it fails" <|
        \() ->
          Matchers.hasBody "{\"name\":\"cool\"}" (requestWithBody "{}")
            |> Expect.equal (Expect.fail (format [fact "Expected request to have body" "{\"name\":\"cool\"}", fact "but it has" "{}"]))
      ]
    , describe "when the body matches"
      [ test "it passes" <|
        \() ->
          Matchers.hasBody "{}" (requestWithBody "{}")
            |> Expect.equal Expect.pass
      ]
    ]
  ]


getWithQuery : Maybe String -> HttpRequest
getWithQuery maybeQuery =
  Http.request
    { method = "GET"
    , headers = []
    , url = "http://fun.com/fun" ++ (Maybe.withDefault "" maybeQuery)
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }
  |> HttpInternal.makeHttpRequest


hasQueryParamTests : Test
hasQueryParamTests =
  describe "hasQueryParam"
  [ describe "when there is no query string"
    [ test "it fails" <|
      \() ->
        Matchers.hasQueryParam ( "name", "fun person" ) (getWithQuery Nothing)
          |> Expect.equal (Expect.fail (format [ fact "Expected request to have query param" "name = fun person", note "but it has no query string" ]))
    ]
  , describe "when there is a query string"
    [ describe "when the param is not present"
      [ test "it fails and reports the actual raw query string" <|
        \() ->
          Matchers.hasQueryParam ( "name", "fun person" ) (getWithQuery (Just "?name=fun%20persons"))
            |> Expect.equal (Expect.fail (format [ fact "Expected request to have query param" "name = fun person", fact "but it has" "name=fun%20persons" ]))
      ]
    , describe "when the param is present"
      [ test "it passes" <|
        \() ->
          Matchers.hasQueryParam ( "name", "fun person" ) (getWithQuery (Just "?name=fun%20person"))
            |> Expect.equal Expect.pass
      ]
    ]
  ]

getWithHeader : Maybe { name: String, value: String } -> HttpRequest
getWithHeader maybeHeader =
  let
    headers = 
      case maybeHeader of
        Just header ->
          [ Http.header header.name header.value ]
        Nothing ->
          []
  in
    Http.request
      { method = "GET"
      , headers = headers
      , url = "http://fun.com/fun.html"
      , body = Http.emptyBody
      , expect = Http.expectString
      , timeout = Nothing
      , withCredentials = False
      }
    |> HttpInternal.makeHttpRequest
  

hasHeaderTests : Test
hasHeaderTests =
  describe "hasHeader"
  [ describe "when there are no headers"
    [ test "it fails" <|
      \() ->
        Matchers.hasHeader ("my-header", "my-header-value") (getWithHeader Nothing)
          |> Expect.equal (Expect.fail (format [ fact "Expected request to have header" "my-header = my-header-value", note "but no headers have been set" ]))
    ]
  , describe "when there is no header with the key"
    [ test "it fails" <|
      \() ->
        Matchers.hasHeader ("my-header", "my-header-value") (getWithHeader (Just { name = "some-header", value = "some-header-value" }))
          |> Expect.equal (Expect.fail (format [ fact "Expected request to have header" "my-header = my-header-value", fact "but it has" "some-header = some-header-value" ]))
    ]
  , describe "where there is a header with the key but a different value"
    [ test "it fails" <|
      \() ->
        Matchers.hasHeader ("my-header", "my-header-value") (getWithHeader (Just { name = "my-header", value = "some-header-value" }))
          |> Expect.equal (Expect.fail (format [ fact "Expected request to have header" "my-header = my-header-value", fact "but it has" "my-header = some-header-value" ]))
    ]
  , describe "when a header key and value matches"
    [ test "it passes" <|
      \() ->
        Matchers.hasHeader ("my-header", "my-header-value") (getWithHeader (Just { name = "my-header", value = "my-header-value" }))
          |> Expect.equal Expect.pass
    ]
  ]
