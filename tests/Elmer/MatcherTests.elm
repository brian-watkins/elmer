module Elmer.MatcherTests exposing (all)

import Test exposing (..)
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Matchers as Matchers

all : Test
all =
  describe "Matcher Tests"
    [ hasTextTests
    , hasClassTests
    ]

hasTextTests : Test
hasTextTests =
  describe "hasText"
  [ describe "when the element has no text"
    [ test "it fails with the right message" <|
      \() ->
        Matchers.hasText "Some text" (emptyNode "div")
          |> Expect.equal (Expect.fail "Expected node to have text\n\n\tSome text\n\nbut it has no text")
    ]
  , describe "when the element has the wrong text"
    [ test "it fails with the right message" <|
      \() ->
        Matchers.hasText "Some text" (nodeWithText "other text")
          |> Expect.equal (Expect.fail "Expected node to have text\n\n\tSome text\n\nbut it has\n\n\tother text")
    ]
  , describe "when the element has the text"
    [ test "it passes" <|
      \() ->
        Matchers.hasText "Some text" (nodeWithText "Some text")
          |> Expect.equal Expect.pass
    ]
  , describe "when the element has multiple text nodes, one of which has the text"
    [ test "it passes" <|
      \() ->
        Matchers.hasText "Some text" (nodeWithMultipleChildren "Some text")
          |> Expect.equal Expect.pass
    ]
    , describe "when the element has multiple text nodes, none of which has the text"
      [ test "it fails with the right message" <|
        \() ->
          Matchers.hasText "Other stuff" (nodeWithMultipleChildren "Some text")
            |> Expect.equal (Expect.fail "Expected node to have text\n\n\tOther stuff\n\nbut it has\n\n\tfun stuff, Some text")
      ]
    , describe "when the text is in a child node"
      [ test "it finds the text" <|
        \() ->
          Matchers.hasText "Child Text" (nodeWithNestedChildren "Child Text")
            |> Expect.equal Expect.pass
      ]
  ]

hasClassTests : Test
hasClassTests =
  describe "hasClass"
  [ describe "when the element has no classes"
    [ test "it fails with the right message" <|
      \() ->
        Matchers.hasClass "myClass" (emptyNode "div")
          |> Expect.equal (Expect.fail "Expected node to have class\n\n\tmyClass\n\nbut it has no classes")
    ]
  , describe "when the element has classes"
    [ describe "when the element does not have the specified class"
      [ test "it returns fails with the right message" <|
        \() ->
          Matchers.hasClass "myClass" (nodeWithClass "anotherClass")
            |> Expect.equal (Expect.fail "Expected node to have class\n\n\tmyClass\n\nbut it has\n\n\tanotherClass, funClass")
      ]
    , describe "when the element has the specified class"
      [ test "it passes" <|
        \() ->
          Matchers.hasClass "myClass" (nodeWithClass "myClass")
            |> Expect.equal Expect.pass
      ]
    ]
  ]
