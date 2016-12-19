module Elmer.HttpMatcherTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Types exposing (..)
import Elmer.Http.Matchers as Matchers

all : Test
all =
  describe "http matcher tests"
  [ hasAnyBodyTests
  , hasBodyTests
  , existsTests
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
          |> Expect.equal (Expect.fail "Expected request to have body\n\n\t{}\n\nbut it has no body")
    ]
  , describe "when there is a body"
    [ describe "when the body does not match"
      [ test "it fails" <|
        \() ->
          Matchers.hasBody "{\"name\":\"cool\"}" (requestWithBody "{}")
            |> Expect.equal (Expect.fail "Expected request to have body\n\n\t{\"name\":\"cool\"}\n\nbut it has\n\n\t{}")
      ]
    , describe "when the body matches"
      [ test "it passes" <|
        \() ->
          Matchers.hasBody "{}" (requestWithBody "{}")
            |> Expect.equal Expect.pass
      ]
    ]
  ]

existsTests : Test
existsTests =
  describe "exists"
  [ describe "when the request has been made"
    [ test "it passes" <|
      \() ->
        Matchers.exists requestWithNoBody
          |> Expect.equal Expect.pass
    ]
  ]
