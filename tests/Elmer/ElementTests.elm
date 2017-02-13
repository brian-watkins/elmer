module Elmer.ElementTests exposing (all)

import Test exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Html.Element as Element
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Elmer.TestHelpers exposing (..)

all : Test
all =
  describe "element tests"
  [ classListTests
  , idTests
  , propertyTests
  , toStringTests
  ]


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

    ]
  ]

toStringTests : Test
toStringTests =
  describe "toString"
  [ test "it prints a node" <|
    \() ->
      let
        nodeResult = Native.Html.asHtmlElement sampleHtml
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
            Expect.fail "Expected a node"
  ]

type Msg
  = Click

sampleHtml : Html Msg
sampleHtml =
  Html.div [ Attr.id "title", Attr.class "myClass" ]
  [ Html.p [ Attr.class "button", Events.onClick Click ] [ Html.text "Some text" ]
  , Html.p [ Attr.class "description", Attr.attribute "data-fun-stuff" "bowling" ] [ Html.text "More text" ]
  ]
