module Elmer.MatcherTests exposing (all)

import Test exposing (..)
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Html exposing (HtmlElement)
import Elmer.Html.Matchers as Matchers
import Elmer.Printer exposing (..)
import Html exposing (Html)
import Html.Attributes as Attr

all : Test
all =
  describe "Matcher Tests"
    [ hasTextTests
    , hasClassTests
    , hasPropertyTests
    , hasIdTests
    , hasStyleTests
    , hasAttributeTests
    , listensForEventTests
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
          |> Expect.equal (Expect.fail "Expected element to have property\n\n\tinnerHTML = some <i>html</i>\n\nbut it has no property with that name")
    ]
  , describe "when the node has properties"
    [ describe "when the node does not have the specified property"
      [ test "it fails with the right message" <|
        \() ->
          Matchers.hasProperty ("innerHTML", "some <i>html</i>") (nodeWithProperty ("someProperty", "blah"))
            |> Expect.equal (Expect.fail "Expected element to have property\n\n\tinnerHTML = some <i>html</i>\n\nbut it has no property with that name")
      ]
    , describe "when the node has the specified property"
      [ describe "when the value is incorrect"
        [ test "it fails" <|
          \() ->
            Matchers.hasProperty ("innerHTML", "some <i>html</i>") (nodeWithProperty ("innerHTML", "blah"))
              |> Expect.equal (Expect.fail "Expected element to have property\n\n\tinnerHTML = some <i>html</i>\n\nbut it has\n\n\tinnerHTML = blah")
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

elementWithAttributes : List (String, String) -> HtmlElement msg
elementWithAttributes attributes =
  let
    attrs = List.map (\(name, value) -> Attr.attribute name value) attributes
    html = Html.div attrs []
  in
    Native.Html.asHtmlElement html
      |> Maybe.withDefault (nodeWithId "fail")


hasAttributeTests : Test
hasAttributeTests =
  describe "hasAttribute"
  [ describe "when the node has no attributes"
    [ test "it fails with the right message" <|
      \() ->
        Matchers.hasAttribute ("data-fun-attribute", "something") (emptyNode "div")
          |> Expect.equal (Expect.fail (format [ message "Expected element to have attribute" "data-fun-attribute = something", description "but it has no attribute with that name" ]))
    ]
  , describe "when the element has attributes"
    [ describe "when the node does not have the specified attribute"
      [ test "it fails with the right message" <|
        \() ->
          Matchers.hasAttribute ("data-fun-attribute", "something") (elementWithAttributes [("someProperty", "blah")])
            |> Expect.equal (Expect.fail (format [ message "Expected element to have attribute" "data-fun-attribute = something", description "but it has no attribute with that name" ]))
      ]
    , describe "when the element has the specified attribute"
      [ describe "when the value is incorrect"
        [ test "it fails" <|
          \() ->
            Matchers.hasAttribute ("data-fun-attribute", "something") (elementWithAttributes [("data-fun-attribute", "blah")])
              |> Expect.equal (Expect.fail ( format [ message "Expected element to have attribute" "data-fun-attribute = something", message "but it has" "data-fun-attribute = blah" ]))
        ]
      , describe "when the value is correct"
        [ test "it passes" <|
          \() ->
            Matchers.hasAttribute ("data-fun-attribute", "something") (elementWithAttributes [("data-fun-attribute", "something")])
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

hasStyleTests : Test
hasStyleTests =
  describe "hasStyle"
  [ describe "when the element has no style"
    [ test "it fails" <|
      \() ->
        Matchers.hasStyle ("position", "relative") (emptyNode "div")
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected element to have style" "position: relative"
              , description "but it has no style"
              ]
            )
    ]
  , describe "when the element has some other style"
    [ test "it fails" <|
      \() ->
        Matchers.hasStyle ("position", "relative") (elementWithStyles [ ("left", "0px"), ("top", "20px") ])
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected element to have style" "position: relative"
              , message "but it has style" "left: 0px\ntop: 20px"
              ]
            )
    ]
  , describe "when the element has the style name but not the style value"
    [ test "it fails" <|
      \() ->
        Matchers.hasStyle ("position", "relative") (elementWithStyles [("position", "absolute")])
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected element to have style" "position: relative"
              , message "but it has style" "position: absolute"
              ]
            )

    ]
  , describe "when the element has the style"
    [ test "it passes" <|
      \() ->
        Matchers.hasStyle ("position", "relative") (elementWithStyles [("margin", "10px"), ("position", "relative")])
          |> Expect.equal Expect.pass
    ]
  ]

elementWithStyles : List (String, String) -> HtmlElement msg
elementWithStyles styles =
  let
    html = Html.div [ Attr.style styles ] []
  in
    Native.Html.asHtmlElement html
      |> Maybe.withDefault (nodeWithId "fail")


listensForEventTests : Test
listensForEventTests =
  describe "listensForEvent"
  [ describe "when the element has no event listeners"
    [ test "it fails" <|
      \() ->
        Matchers.listensForEvent "click" (emptyNode "div")
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected element to listen for event" "click"
              , description "but it has no event listeners"
              ]
            )
    ]
  , describe "when the element does not have the correct event listener"
    [ test "it fails" <|
      \() ->
        Matchers.listensForEvent "click" (nodeWithEvents [ "mouseup", "mousedown" ])
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected element to listen for event" "click"
              , message "but it listens for" "mouseup\nmousedown"
              ]
            )
    ]
  , describe "when the element has the expected event listener"
    [ test "it passes" <|
      \() ->
        Matchers.listensForEvent "click" (nodeWithEvents [ "mouseup", "click", "mousedown" ])
          |> Expect.equal Expect.pass
    ]
  ]
