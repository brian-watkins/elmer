module Elmer.ElementTests exposing (..)

import Test exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Html.Element as Element
import Elmer.Html.Node as Node
import Elmer.Html.Matchers as Matchers
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Elmer.TestHelpers exposing (..)


classListTests : Test
classListTests =
  describe "classList"
  [ describe "when the node has no classes"
    [ test "it returns an empty array" <|
      \() ->
        Element.classList (emptyNode "div")
          |> Expect.equal []
    ]
  , describe "when the node has classes"
    [ test "it returns an array of classes" <|
      \() ->
        Element.classList (nodeWithClass "title")
          |> Expect.equal [ "title", "funClass" ]
    ]
  ]

idTests : Test
idTests =
  describe "id"
  [ describe "when the node has no id"
    [ test "it retuns Nothing" <|
      \() ->
        Element.id (emptyNode "div")
          |> Expect.equal Nothing
    ]
  , describe "when the node has an id"
    [ test "it returns the id" <|
      \() ->
        Element.id (nodeWithId "fun")
          |> Expect.equal (Just "fun")
    ]
  ]

propertyTests : Test
propertyTests =
  describe "property"
  [ describe "when the node has no properties" <|
    [ test "it returns nothing" <|
      \() ->
        Element.property "innerHTML" (emptyNode "div")
          |> Expect.equal Nothing
    ]
  , describe "when the node has properties"
    [ describe "when the node does not have the requested property"
      [ test "it returns nothing" <|
        \() ->
          Element.property "innerHTML" (nodeWithProperty ("blah", "blah"))
            |> Expect.equal Nothing
      ]
    , describe "when the node has the requested property"
      [ test "it returns the property" <|
        \() ->
          Element.property "innerHTML" (nodeWithProperty ("innerHTML", "blah"))
            |> Expect.equal (Just "blah")
      ]
    , describe "when the node has a boolean property value"
      [ test "it returns the property value" <|
        \() ->
          Element.property "checked" (nodeWithBooleanProperty ("checked", True))
            |> Expect.equal (Just "true")
      ]
    ]
  ]

boolPropertyTests : Test
boolPropertyTests =
  describe "boolProperty"
  [ describe "when the node has no properties" <|
    [ test "it returns nothing" <|
      \() ->
        Element.boolProperty "someProperty" (emptyNode "div")
          |> Expect.equal Nothing
    ]
  , describe "when the node has properties"
    [ describe "when the node does not have the requested property"
      [ test "it returns nothing" <|
        \() ->
          Element.boolProperty "someProperty" (nodeWithProperty ("blah", "blah"))
            |> Expect.equal Nothing
      ]
    , describe "when the node has the requested property"
      [ describe "when the property does not have a boolean value"
        [ test "it returns nothing" <|
          \() ->
            Element.boolProperty "innerHTML" (nodeWithProperty ("innerHTML", "blah"))
              |> Expect.equal Nothing
        ]
      , describe "when the property has a boolean value"
        [ test "it returns the property value" <|
          \() ->
            Element.boolProperty "checked" (nodeWithBooleanProperty ("checked", True))
              |> Expect.equal (Just True)
        ]
      ]
    ]
  ]

targetTests : Test
targetTests =
  let
    nodeResult =
      Node.from sampleHtml
        |> Node.asElement
  in
  describe "target"
  [ test "it targets a child" <|
    \() ->
      case nodeResult of
        Just element ->
          element
            |> Element.target ".description"
            |> Matchers.element (
              Matchers.hasText "More text"
            )
        Nothing ->
          Expect.fail "Expected an element"
  , test "it targets multiple children" <|
    \() ->
      case nodeResult of
        Just element ->
          element
            |> Element.target "p"
            |> Matchers.elements (
              Elmer.hasLength 2
            )
        Nothing ->
          Expect.fail "Expected an element"
  , test "it fails when the selector matches no children" <|
    \() ->
      case nodeResult of
        Just element ->
          element
            |> Element.target "#matches-nothing"
            |> (Elmer.expectNot <| Matchers.elementExists)
        Nothing ->
          Expect.fail "Expected an element"
  ]


toStringTests : Test
toStringTests =
  describe "toString"
  [ test "it prints a node" <|
    \() ->
      let
        nodeResult =
          Node.from sampleHtml
            |> Node.asElement
      in
        case nodeResult of
          Just node ->
            let
              expected = "- div { className = 'myClass', id = 'title' } \n"
                  ++ "  - p { className = 'button' } [ click ]\n"
                  ++ "    - Some text\n"
                  ++ "  - p { data-fun-stuff = 'bowling', className = 'description' } \n"
                  ++ "    - More text"
            in
            Expect.equal (Element.toString node) expected
          Nothing ->
            Expect.fail "Expected an element"
  , test "it prints a node with a boolean property" <|
    \() ->
      let
        elementResult =
          Node.from htmlWithBoolProperty
            |> Node.asElement
      in
        case elementResult of
          Just element ->
            let
              expected = "- form { id = 'my-form' } \n"
                ++ "  - input { checked = true, type = 'checkbox' } [ change ]\n"
                ++ "    - Check me!"
            in
              Expect.equal (Element.toString element) expected
          Nothing ->
            Expect.fail "Expected an element"
  ]

type Msg
  = Click
  | Check Bool

sampleHtml : Html Msg
sampleHtml =
  Html.div [ Attr.id "title", Attr.class "myClass" ]
  [ Html.p [ Attr.class "button", Events.onClick Click ] [ Html.text "Some text" ]
  , Html.p [ Attr.class "description", Attr.attribute "data-fun-stuff" "bowling" ] [ Html.text "More text" ]
  ]

htmlWithBoolProperty : Html Msg
htmlWithBoolProperty =
  Html.form [ Attr.id "my-form" ]
  [ Html.input
    [ Attr.type_ "checkbox", Attr.checked True, Events.onCheck Check ]
    [ Html.text "Check me!" ]
  ]
