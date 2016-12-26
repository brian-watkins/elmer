module Elmer.HttpMatcherTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Types exposing (..)
import Elmer.Http.Matchers as Matchers
import Elmer.Printer exposing (..)

all : Test
all =
  describe "http matcher tests"
  [ hasAnyBodyTests
  , hasBodyTests
  , hasBeenRequestedTests
  , hasQueryParamTests
  ]

requestWithBody : String -> HttpRequestData
requestWithBody body =
  { method = "POST"
  , url = "http://fun.com/fun"
  , body = Just body
  }

requestWithNoBody : HttpRequestData
requestWithNoBody =
  { method = "POST"
  , url = "http://fun.com/fun"
  , body = Nothing
  }

hasAnyBodyTests : Test
hasAnyBodyTests =
  describe "hasAnyBody"
  [ describe "when there is no body"
    [ test "it fails" <|
      \() ->
        Matchers.hasAnyBody requestWithNoBody
          |> Expect.equal (Expect.fail "Expected request to have a body but it does not")
    ]
  , describe "when there is a body"
    [ test "it passes" <|
      \() ->
        Matchers.hasAnyBody (requestWithBody "{}")
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
          |> Expect.equal (Expect.fail (format [message "Expected request to have body" "{}", description "but it has no body"]))
    ]
  , describe "when there is a body"
    [ describe "when the body does not match"
      [ test "it fails" <|
        \() ->
          Matchers.hasBody "{\"name\":\"cool\"}" (requestWithBody "{}")
            |> Expect.equal (Expect.fail (format [message "Expected request to have body" "{\"name\":\"cool\"}", message "but it has" "{}"]))
      ]
    , describe "when the body matches"
      [ test "it passes" <|
        \() ->
          Matchers.hasBody "{}" (requestWithBody "{}")
            |> Expect.equal Expect.pass
      ]
    ]
  ]

hasBeenRequestedTests : Test
hasBeenRequestedTests =
  describe "hasBeenRequested"
  [ describe "when the request has been made"
    [ test "it passes" <|
      \() ->
        Matchers.hasBeenRequested requestWithNoBody
          |> Expect.equal Expect.pass
    ]
  ]

getWithQuery : Maybe String -> HttpRequestData
getWithQuery maybeQuery =
  { method = "GET"
  , url = "http://fun.com/fun" ++ (Maybe.withDefault "" maybeQuery)
  , body = Nothing
  }

hasQueryParamTests : Test
hasQueryParamTests =
  describe "hasQueryParam"
  [ describe "when there is no query string"
    [ test "it fails" <|
      \() ->
        Matchers.hasQueryParam ( "name", "fun person" ) (getWithQuery Nothing)
          |> Expect.equal (Expect.fail (format [ message "Expected request to have query param" "name = fun person", description "but it has no query string" ]))
    ]
  , describe "when there is a query string"
    [ describe "when the param is not present"
      [ test "it fails" <|
        \() ->
          Matchers.hasQueryParam ( "name", "fun person" ) (getWithQuery (Just "?something=else"))
            |> Expect.equal (Expect.fail (format [ message "Expected request to have query param" "name = fun person", message "but it has" "something=else" ]))
      , test "it fails" <|
        \() ->
          Matchers.hasQueryParam ( "name", "fun person" ) (getWithQuery (Just "?name=fun%20persons"))
            |> Expect.equal (Expect.fail (format [ message "Expected request to have query param" "name = fun person", message "but it has" "name=fun%20persons" ]))
      ]
    , describe "when the param is present"
      [ test "it passes" <|
        \() ->
          Matchers.hasQueryParam ( "name", "fun person" ) (getWithQuery (Just "?name=fun%20person"))
            |> Expect.equal Expect.pass
      ]
    ]
  ]
