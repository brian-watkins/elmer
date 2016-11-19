module Elmer.TestExtraTests exposing (all)

import Test exposing (..)
import Elmer.TestExtra as ElmerTest
import Expect

all : Test
all =
  describe "TestExtra Tests"
    [ notTests
    , andTests
    ]


notTests =
  describe "not"
  [ describe "when there is a failure"
    [ test "it passes" <|
      \() ->
        ElmerTest.not (Expect.fail "failure")
          |> Expect.equal Expect.pass
    ]
  , describe "when there is a pass"
    [ test "it fails" <|
      \() ->
        ElmerTest.not Expect.pass
          |> Expect.equal (Expect.fail "Expected to fail, but it passed")
    ]
  ]


andTests =
  describe "andThen"
  [ describe "when there is a failure in the first expectation"
    [ test "it fails without exercising the second expectation" <|
      \() ->
        let
          result = ElmerTest.andThen (Expect.fail "I failed") (Expect.fail "Me too")
        in
          Expect.equal (Expect.fail "I failed") result
    ]
  , describe "when the first expectation passes"
    [ test "it returns the second expectation" <|
      \() ->
        let
          result = Expect.pass
            |> ElmerTest.andThen (Expect.fail "I failed")
        in
          Expect.equal (Expect.fail "I failed") result
    ]
  , describe "when there are multiple andThen's"
    [ test "it returns any failure along the way" <|
      \() ->
        let
          result = Expect.pass
            |> ElmerTest.andThen Expect.pass
            |> ElmerTest.andThen (Expect.fail "Whoops!")
            |> ElmerTest.andThen Expect.pass
        in
          Expect.equal (Expect.fail "Whoops!") result
    ]
  ]
