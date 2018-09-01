module Elmer.HtmlMatcherTests exposing (..)

import Test exposing (..)
import Elmer.TestHelpers exposing (..)
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Expect
import Elmer exposing (..)
import Elmer.Html
import Elmer.Html.Matchers as Matchers
import Elmer.Html.Query as Query
import Elmer.Html.Node as Node
import Elmer.Html.Types exposing (..)
import Elmer.Html.Printer as HtmlPrinter
import Elmer.Printer exposing (..)
import Elmer.Errors as Errors
import Elmer.TestHelpers exposing (printHtml)
import Html exposing (Html, Attribute)
import Html.Attributes as Attr


all : Test
all =
  Test.concat
  [ elementTests
  , elementsTests
  , elementExistsTests
  , hasTextTests
  , hasClassTests
  , hasPropertyTests
  , hasAttributeTests
  , hasIdTests
  , hasStyleTests
  , listensForEventTests
  ]

testHtmlContext : String -> HtmlTarget SimpleApp.Msg
testHtmlContext selector =
  Query.forHtml selector <| SimpleApp.view SimpleApp.defaultModel

testTextHtmlContext : String -> HtmlTarget SimpleApp.Msg
testTextHtmlContext selector =
  Query.forHtml selector <| SimpleApp.textView SimpleApp.defaultModel

testChildrenHtmlContext : String -> HtmlTarget SimpleApp.Msg
testChildrenHtmlContext selector =
  Query.forHtml selector <| SimpleApp.viewWithChildren SimpleApp.defaultModel


elementTests : Test
elementTests =
  describe "element"
  [ describe "when the targeted element does not exist"
    [ test "it returns the failure message and prints the view" <|
      \() ->
        Matchers.element (\el -> Expect.fail "Should not get here") (testHtmlContext ".blah")
          |> Expect.equal (Expect.fail <|
            Errors.elementNotFound ".blah" (printHtml <| SimpleApp.view SimpleApp.defaultModel)
          )
    ]
  , describe "when there are no elements in the html"
    [ test "it shows there are no elements found" <|
      \() ->
        Matchers.element (\el -> Expect.fail "Should not get here") (testTextHtmlContext ".blah")
          |> Expect.equal (Expect.fail <|
            Errors.elementNotFound ".blah" (printHtml <| SimpleApp.textView SimpleApp.defaultModel)
          )
    ]
  , describe "when the targeted element exists"
    [ test "it passes the element to the matcher" <|
      \() ->
        Matchers.element (Matchers.hasText "Some text") (testHtmlContext "#root")
          |> Expect.equal Expect.pass
    ]
  ]

elementsTests : Test
elementsTests =
  describe "elements"
  [ describe "when the targeted element does not exist"
    [ test "it passes an empty list to the matcher" <|
      \() ->
        testChildrenHtmlContext "blah"
          |> Matchers.elements (\els ->
            Expect.equal True <| List.isEmpty els
          )
          |> Expect.equal Expect.pass
    ]
  , describe "when the targeted element exists"
    [ test "it passes the matching elements to the matcher" <|
      \() ->
        testChildrenHtmlContext "div"
          |> Matchers.elements (\els ->
            Expect.equal 4 <| List.length els
          )
          |> Expect.equal Expect.pass
    ]
  ]

elementExistsTests : Test
elementExistsTests =
  describe "elementExists"
  [ describe "when the targeted element does not exist"
    [ test "it fails" <|
      \() ->
        Matchers.elementExists (testHtmlContext ".blah")
          |> Expect.equal (Expect.fail <|
            Errors.elementNotFound ".blah" (printHtml <| SimpleApp.view SimpleApp.defaultModel)
          )
    ]
  , describe "when the targeted element exists"
    [ test "it passes" <|
      \() ->
        Matchers.elementExists (testHtmlContext "#root")
          |> Expect.equal Expect.pass
    ]
  ]


hasTextTests : Test
hasTextTests =
  describe "hasText"
  [ describe "when the element has no text"
    [ test "it fails with the right message" <|
      \() ->
        Matchers.hasText "Some text" (emptyNode "div")
          |> Expect.equal (Expect.fail "Expected element to have text\n\n\tSome text\n\nbut it has no text")
    ]
  , describe "when the element has the wrong text"
    [ test "it fails with the right message" <|
      \() ->
        Matchers.hasText "Some text" (nodeWithText "other text")
          |> Expect.equal (Expect.fail "Expected element to have text\n\n\tSome text\n\nbut it has\n\n\tother text")
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
            |> Expect.equal (Expect.fail "Expected element to have text\n\n\tOther stuff\n\nbut it has\n\n\tfun stuff, Some text")
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
          |> Expect.equal (Expect.fail "Expected element to have class\n\n\tmyClass\n\nbut it has no classes")
    ]
  , describe "when the element has classes"
    [ describe "when the element does not have the specified class"
      [ test "it fails with the right message" <|
        \() ->
          Matchers.hasClass "myClass" (nodeWithClass "anotherClass")
            |> Expect.equal (Expect.fail "Expected element to have class\n\n\tmyClass\n\nbut it has\n\n\tanotherClass, funClass")
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
    Node.from html
      |> Node.asElement
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
          |> Expect.equal (Expect.fail "Expected element to have id\n\n\troot\n\nbut it has no id")
    ]
  , describe "when the node has an id"
    [ describe "when the id does not match"
      [ test "it fails" <|
        \() ->
          Matchers.hasId "root" (nodeWithId "blah")
            |> Expect.equal (Expect.fail "Expected element to have id\n\n\troot\n\nbut it has id\n\n\tblah")
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
    html = Html.div (styleAttributes styles) []
  in
    Node.from html
      |> Node.asElement
      |> Maybe.withDefault (nodeWithId "fail")

styleAttributes : List (String, String) -> List (Attribute msg)
styleAttributes =
  List.map (\style -> Attr.style (Tuple.first style) (Tuple.second style))

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
