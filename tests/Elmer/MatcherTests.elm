module Elmer.MatcherTests exposing (all)

import Test exposing (..)
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Matchers as Matchers exposing ((<&&>))

all : Test
all =
  describe "Matcher Tests"
    [ hasTextTests
    , hasClassTests
    , hasPropertyTests
    , hasIdTests
    , andThenTests
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
      [ test "it fails with the right message" <|
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

hasPropertyTests : Test
hasPropertyTests =
  describe "hasProperty"
  [ describe "when the node has no properties"
    [ test "it fails with the right message" <|
      \() ->
        Matchers.hasProperty ("innerHTML", "some <i>html</i>") (emptyNode "div")
          |> Expect.equal (Expect.fail "Expected node to have property\n\n\tinnerHTML = some <i>html</i>\n\nbut it has no property with that name")
    ]
  , describe "when the node has properties"
    [ describe "when the node does not have the specified property"
      [ test "it fails with the right message" <|
        \() ->
          Matchers.hasProperty ("innerHTML", "some <i>html</i>") (nodeWithProperty ("someProperty", "blah"))
            |> Expect.equal (Expect.fail "Expected node to have property\n\n\tinnerHTML = some <i>html</i>\n\nbut it has no property with that name")
      ]
    , describe "when the node has the specified property"
      [ describe "when the value is incorrect"
        [ test "it fails" <|
          \() ->
            Matchers.hasProperty ("innerHTML", "some <i>html</i>") (nodeWithProperty ("innerHTML", "blah"))
              |> Expect.equal (Expect.fail "Expected node to have property\n\n\tinnerHTML = some <i>html</i>\n\nbut it has\n\n\tinnerHTML = blah")
        ]
      , describe "when the value is correct"
        [ test "it passes" <|
          \() ->
            Matchers.hasProperty ("innerHTML", "some <i>html</i>") (nodeWithProperty ("innerHTML", "some <i>html</i>"))
              |> Expect.equal Expect.pass
        ]
      ]
    ]
  ]

hasIdTests : Test
hasIdTests =
  describe "hasId"
  [ describe "when the node has no id"
    [ test "it fails with the right message" <|
      \() ->
        Matchers.hasId "root" (emptyNode "div")
          |> Expect.equal (Expect.fail "Expected node to have id\n\n\troot\n\nbut it has no id")
    ]
  , describe "when the node has an id"
    [ describe "when the id does not match"
      [ test "it fails" <|
        \() ->
          Matchers.hasId "root" (nodeWithId "blah")
            |> Expect.equal (Expect.fail "Expected node to have id\n\n\troot\n\nbut it has id\n\n\tblah")
      ]
    , describe "when the id matches"
      [ test "it passes" <|
        \() ->
          Matchers.hasId "root" (nodeWithId "root")
            |> Expect.equal (Expect.pass)
      ]
    ]
  ]

andThenTests : Test
andThenTests =
  describe "andThen"
  [ describe "when all matchers pass"
    [ test "it passes" <|
      \() ->
        (nodeWithClassAndId "myClass" "myId") |>
          Matchers.hasId "myId"
            <&&> Matchers.hasClass "myClass"
            <&&> Matchers.hasClass "funClass"
    ]
  , describe "when the first matcher fails"
    [ test "it fails with the first failure" <|
      \() ->
        (nodeWithClass "myClass")
          |> (Matchers.hasId "root"
                <&&> Matchers.hasClass "myClass")
          |> Expect.equal (Expect.fail "Expected node to have id\n\n\troot\n\nbut it has no id")
    ]
  , describe "when the second matcher fails"
    [ test "it fails with the second failure" <|
      \() ->
        (nodeWithId "root")
          |> (Matchers.hasId "root"
                <&&> Matchers.hasClass "myClass")
          |> Expect.equal (Expect.fail "Expected node to have class\n\n\tmyClass\n\nbut it has no classes")
    ]
  ]
