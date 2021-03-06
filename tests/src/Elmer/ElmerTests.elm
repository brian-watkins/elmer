module Elmer.ElmerTests exposing (..)

import Test exposing (..)
import Test.Runner
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Html
import Elmer.Html.Matchers as Matchers exposing (element, hasText)
import Elmer.Spy as Spy
import Elmer.Command as Command
import Elmer.Message exposing (..)
import Task
import Time


all : Test
all =
  Test.concat
  [ matchAllTests
  , matchOneTests
  , matchExactlyTests
  , lastTests
  , atIndexTests
  , hasLengthTests
  , expectNotTests
  , expectAllTests
  , expectModelTests
  ]


matchAllTests : Test
matchAllTests =
  describe "each"
  [ describe "when all items match"
    [ test "it passes" <|
      \() ->
        let
          items = [ 2, 4, 6, 8, 10 ]
        in
          each (\n -> Expect.equal (modBy 2 n) 0) items
            |> Expect.equal Expect.pass
    ]
  , describe "when one item fails to match"
    [ test "it fails" <|
      \() ->
        let
          items = [ 2, 4, 5, 6, 8, 9, 10]
        in
          each (\n -> Expect.equal (modBy 2 n) 0) items
            |> Expect.equal (Expect.fail <| format
              [ note "Expected all to pass but some failed:"
              , note "1 is not equal to 0\n\n1 is not equal to 0"
              ]
            )
    ]
  , describe "when the list is empty"
    [ test "it fails" <|
      \() ->
        each (Expect.equal 2) []
          |> Expect.equal (Expect.fail <| format
            [ note "Expected all to pass but the list is empty"]
          )
    ]
  ]

matchOneTests : Test
matchOneTests =
  describe "some"
    [ describe "when no items match"
      [ test "it fails" <|
        \() ->
          let
            items = [ 2, 4 ]
          in
            some (\n -> Expect.equal (modBy 17 n) 0) items
              |> Expect.equal (Expect.fail <| format
                [ note "Expected some to pass but found none. Here are the failures:"
                , note "2 is not equal to 0\n\n4 is not equal to 0"
                ]
              )
      ]
    , describe "when one item matches"
      [ test "it passes" <|
        \() ->
          let
            items = [ 2, 4, 5, 17, 8, 10]
          in
            some (\n -> Expect.equal (modBy 17 n) 0) items
              |> Expect.equal Expect.pass
      ]
    , describe "when the list is empty"
      [ test "it fails" <|
        \() ->
          some (Expect.equal 3) []
            |> Expect.equal (Expect.fail <| format
              [ note "Expected some to pass but the list is empty" ]
            )
      ]
    ]

matchExactlyTests : Test
matchExactlyTests =
  describe "exactly"
  [ describe "when exactly the right number of items match"
    [ test "it passes" <|
      \() ->
        let
          items = [ 2, 4, 4, 6, 4, 8, 10 ]
        in
          exactly 3 (\n -> Expect.equal 4 n) items
            |> Expect.equal Expect.pass
    ]
  , describe "when an unexpected number of items match"
    [ test "it fails" <|
      \() ->
        let
          items = [ 2, 4, 4, 6, 4 ]
        in
          exactly 2 (\n -> Expect.equal 4 n) items
            |> Expect.equal (Expect.fail <| format
              [ note "Expected exactly 2 to pass but found 3. Here are the failures:"
              , note "4 is not equal to 2\n\n4 is not equal to 6"
              ]
            )
    ]
  , describe "when the list is empty"
    [ test "it fails" <|
      \() ->
        let
          items = []
        in
          exactly 2 (Expect.equal 17) items
            |> Expect.equal (Expect.fail <| format
              [ note "Expected exactly 2 to pass but the list is empty"]
            )
    ]
  ]

lastTests : Test
lastTests =
  describe "last"
  [ describe "when the last item matches"
    [ test "it passes" <|
      \() ->
        let
          items = [ 2, 3, 4 ]
        in
          last (Expect.equal 4) items
            |> Expect.equal Expect.pass
    ]
  , describe "when the last item does not match"
    [ test "it fails" <|
      \() ->
        let
          items = [ 2, 3, 4 ]
        in
          last (Expect.equal 7) items
            |> Expect.equal (Expect.fail <| format
              [ note "Expected the last item to pass but it failed:"
              , note "7 is not equal to 4"
              ]
            )
    ]
  , describe "when the list is empty"
    [ test "it fails" <|
      \() ->
        last (Expect.equal 7) []
          |> Expect.equal (Expect.fail <| format
            [ note "Expected the last item to pass but the list is empty" ]
          )
    ]
  ]

atIndexTests : Test
atIndexTests =
  describe "atIndex"
  [ describe "when the item at the index matches"
    [ test "it passes" <|
      \() ->
        let
          items = [ 2, 3, 4 ]
        in
          atIndex 1 (Expect.equal 3) items
            |> Expect.equal Expect.pass
    ]
  , describe "when the item at the index does not match"
    [ test "it fails" <|
      \() ->
        let
          items = [ 2, 3, 4 ]
        in
          atIndex 1 (Expect.equal 2) items
            |> Expect.equal (Expect.fail <| format
              [ note "Expected item at index 1 to pass but it failed:"
              , note "2 is not equal to 3"
              ]
            )
    ]
  , describe "when there is no item at that index"
    [ test "it fails" <|
      \() ->
        let
          items = [ 2, 3, 4 ]
        in
          atIndex 17 (Expect.equal 2) items
            |> Expect.equal (Expect.fail <| format
              [ note "Expected item at index 17 to pass but there is no item at that index"
              ]
            )
    ]
  ]

hasLengthTests : Test
hasLengthTests =
  describe "hasLength"
  [ describe "when the list has the expected size"
    [ test "it passes" <|
      \() ->
        let
          items = [ 2, 4, 6, 8, 10 ]
        in
          hasLength 5 items
            |> Expect.equal Expect.pass
    ]
  , describe "when the list does not have the expected size"
    [ test "it fails" <|
      \() ->
        let
          items = [ 2, 4, 6, 8, 10 ]
        in
          hasLength 3 items
            |> Expect.equal (Expect.fail (format [ fact "Expected list to have size" "3", fact "but it has size" "5"]))
    ]
  ]

expectNotTests : Test
expectNotTests =
  describe "expectNot"
  [ describe "when the matcher passes"
    [ test "it fails" <|
      \() ->
        (nodeWithClassAndId "myClass" "myId")
          |> Elmer.expectNot (Matchers.hasId "myId")
          |> Expect.equal (Expect.fail "Expected not to be the case but it is")
    ]
  , describe "when the matcher fails"
    [ test "it passes" <|
      \() ->
        (nodeWithClassAndId "myClass" "myId")
          |> (Elmer.expectNot <| Matchers.hasId "someWrongId")
          |> Expect.equal Expect.pass
    ]
  ]

expectAllTests : Test
expectAllTests =
  describe "expectAll"
  [ describe "when all matchers pass"
    [ test "it passes" <|
      \() ->
        (nodeWithClassAndId "myClass" "myId") |> 
          expectAll
          [ Matchers.hasId "myId"
          , Matchers.hasClass "myClass"
          , Matchers.hasClass "funClass"
          ]
    ]
  , describe "when the first matcher fails"
    [ test "it fails with the first failure" <|
      \() ->
        (nodeWithClass "myClass")
          |> expectAll [ Matchers.hasId "root", Matchers.hasClass "myClass" ]
          |> Expect.equal (Expect.fail "Expected element to have id\n\n\troot\n\nbut it has no id")
    ]
  , describe "when the second matcher fails"
    [ test "it fails with the second failure" <|
      \() ->
        (nodeWithId "root")
          |> expectAll [ Matchers.hasId "root", Matchers.hasClass "myClass" ]
          |> Expect.equal (Expect.fail "Expected element to have class\n\n\tmyClass\n\nbut it has no classes")
    ]
  ]

expectModelTests : Test
expectModelTests =
  describe "expectModel"
  [ describe "when there is a failure upstream"
    [ test "it fails" <|
      \() ->
        let
          initialState = TestState.failure "You failed!"
        in
          Elmer.expectModel (\model -> Expect.fail "Shouldn't get here") initialState
            |> Expect.equal (Expect.fail "You failed!")
    ]
  , describe "when there is no failure"
    [ test "it runs the matcher on the current model" <|
      \() ->
        Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          |> Elmer.expectModel (\model ->
            Expect.equal model.name "Cool Person"
          )
          |> Expect.equal Expect.pass
    ]
  ]
