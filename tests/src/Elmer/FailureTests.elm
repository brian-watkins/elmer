module Elmer.FailureTests exposing (..)

import Test exposing (..)
import Test.Runner
import Test.Runner.Failure as ElmTestFailure
import Expect
import Elmer.Message.Failure as Failure
import Dict
import Set


all : Test
all =
  Test.concat
  [ formatFailureTests
  ]


takeFailureMessage : Expect.Expectation -> String
takeFailureMessage expectation =
  Test.Runner.getFailureReason expectation
    |>  Maybe.withDefault
          { given = Nothing
          , description = "default"
          , reason = ElmTestFailure.Custom
          }
    |> List.singleton 
    |> Failure.format

formatFailureTests : Test
formatFailureTests =
  describe "format failure"
  [ describe "custom"
    [ test "it prints the custom failure" <|
      \() ->
        Expect.fail "my-failure-description"
          |> takeFailureMessage
          |> Expect.equal "my-failure-description"
    , test "it prints the true failure" <|
      \() ->
        Expect.true "Expect to be true" False
          |> takeFailureMessage
          |> Expect.equal "Expect to be true"
    , test "it prints the false failure" <|
      \() ->
        Expect.false "Expect to be false" True
          |> takeFailureMessage
          |> Expect.equal "Expect to be false"
    ]
  , describe "equality"
    [ test "it prints the equality failure" <|
      \() ->
        Expect.equal 19 20
          |> takeFailureMessage
          |> Expect.equal "19 is not equal to 20"
    , test "it prints the inequality failure" <|
      \() ->
        Expect.notEqual 20 20
          |> takeFailureMessage
          |> Expect.equal "20 is equal to 20"
    ]
  , describe "comparison"
    [ test "it prints the less than failure" <|
      \() ->
        Expect.lessThan 19 20
          |> takeFailureMessage
          |> Expect.equal "20 is not less than 19"
    , test "it prints the at most failure" <|
      \() ->
        Expect.atMost 19 20
          |> takeFailureMessage
          |> Expect.equal "20 is not at most 19"
    , test "it prints the greater than failure" <|
      \() ->
        Expect.greaterThan 20 19
          |> takeFailureMessage
          |> Expect.equal "19 is not greater than 20"
    , test "it prints the at least failure" <|
      \() ->
        Expect.atLeast 20 19
          |> takeFailureMessage
          |> Expect.equal "19 is not at least 20"
    , test "it prints the err failure" <|
      \() ->
        Expect.err (Ok "Blah")
          |> takeFailureMessage
          |> Expect.equal "Ok \"Blah\" is not an Err"
    , test "it prints the unknown comparison failure" <|
      \() ->
        [ { given = Nothing
          , description = "some weird comparison"
          , reason = ElmTestFailure.Comparison "87" "bbb"
          }
        ]
        |> Failure.format
        |> Expect.equal "some weird comparison failed between bbb and 87"
    ]
  , describe "list diff"
    [ test "it prints the list diff failure" <|
      \() ->
        Expect.equalLists [ 1, 2 ] [ 3, 4 ]
          |> takeFailureMessage
          |> Expect.equal "[ 3, 4 ]\n\nis not equal to\n\n[ 1, 2 ]"
    ]
  , describe "collection diff"
    [ test "it prints the equal dicts failure" <|
      \() ->
        Expect.equalDicts (Dict.fromList [ (1, "one") ]) (Dict.fromList [ (2, "two") ])
          |> takeFailureMessage
          |> Expect.equal "Expected\n\nDict.fromList [(1,\"one\")]\n\nbut the actual value is\n\nDict.fromList [(2,\"two\")]"
    , test "it prints the equal sets failure" <|
      \() ->
        Expect.equalSets (Set.fromList [ 1 ]) (Set.fromList [ 2 ])
          |> takeFailureMessage
          |> Expect.equal "Expected\n\nSet.fromList [1]\n\nbut the actual value is\n\nSet.fromList [2]"
    ]
  , describe "failures we don't care about"
    [ test "it prints the full failure reason" <|
      \() ->
        [ { given = Nothing
          , description = "Something"
          , reason = ElmTestFailure.TODO
          } 
        ]
        |> Failure.format
        |> Expect.equal "Failure { description = \"Something\", given = Nothing, reason = TODO }"
    ]
  ]
