module Elmer.NodeTests exposing (all)

import Test exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Node as Node
import Elmer.Matchers as Matchers
import Elmer.Types exposing (..)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Elmer.TestHelpers exposing (..)

all : Test
all =
  describe "node tests"
  [ noElementFound
  , findById
  , findByClass
  , findByTag
  , findByAttribute
  , findByProperty
  , classListTests
  , idTests
  , propertyTests
  , toStringTests
  , findChildrenTests
  ]

noElementFound : Test
noElementFound =
  describe "when no element found"
    [ describe "with bad id"
      [ test "it returns a failure mesage" <|
        \() ->
          let
            html = Html.div [ Attr.id "something" ] []
          in
            Expect.equal ( Node.findNode html "#nothing" ) Nothing
      ]
    , describe "with bad class"
      [ test "it returns nothing" <|
        \() ->
          let
            html = Html.div [ Attr.class "something" ] []
          in
            Expect.equal ( Node.findNode html ".nothing" ) Nothing
      ]
    , describe "when there is only text"
      [ test "it returns nothing" <|
        \() ->
          let
            html = Html.text "Something"
          in
            Expect.equal ( Node.findNode html ".anything" ) Nothing
      ]
    ]

findById : Test
findById =
  let
    html = Html.div [ Attr.id "root" ]
      [ Html.div [ Attr.id "nested" ] []
      ]
  in
    describe "find by id"
      [ test "it finds the top element by id" <|
        \() ->
          case (Node.findNode html "#root") of
            Just a ->
              Matchers.hasId "root" a
            Nothing ->
              Expect.fail "Nothing found"
      , test "finds a nested element by id" <|
        \() ->
          case (Node.findNode html "#nested") of
            Just a ->
              Matchers.hasId "nested" a
            Nothing ->
              Expect.fail "Nothing found"
      ]

findByClass : Test
findByClass =
  describe "find by class"
    [ let
        html = Html.div [ Attr.class "content" ]
          [ Html.div [ Attr.class "nested" ] []
          ]
      in
        describe "when there is one class"
        [ test "it finds the top element by class" <|
          \() ->
            case ( Node.findNode html ".content" ) of
              Just a ->
                Matchers.hasClass "content" a
              Nothing ->
                Expect.fail "Nothing found"
        , test "it finds a nested element by class" <|
          \() ->
            case ( Node.findNode html ".nested" ) of
              Just a ->
                Matchers.hasClass "nested" a
              Nothing ->
                Expect.fail "Nothing found"
        ]
    , let
        html = Html.div [ Attr.classList [ ("awesome", True), ("super", True), ("root", True) ] ] []
      in
        describe "when there is more than one class"
        [ test "it finds the element" <|
          \() ->
            case ( Node.findNode html ".super" ) of
              Just a ->
                Matchers.hasClass "super" a
              Nothing ->
                Expect.fail "Nothing found"
        ]
    , let
        html = Html.div [ Attr.id "root", Attr.class "root" ] []
      in
        describe "when the class name is the same as an id"
        [ test "it returns the element with the class name" <|
          \() ->
            case ( Node.findNode html ".root" ) of
              Just a ->
                Matchers.hasClass "root" a
              Nothing ->
                Expect.fail "Nothing found"
        ]
    , let
        html = Html.div [ Attr.id "root" ]
          [ Html.div [ Attr.id "firstNested" ]
            [ Html.div [ Attr.class "deeplyNested" ] []
            ]
          ]
      in
        describe "when the node is nested"
        [ test "it returns the node with the class name" <|
          \() ->
            case ( Node.findNode html ".deeplyNested" ) of
              Just a ->
                Matchers.hasClass "deeplyNested" a
              Nothing ->
                Expect.fail "Nothing found"
        ]
    ]

findByTag =
  let
    html = Html.div [ Attr.id "root" ]
      [ Html.input [ Attr.class "inputField" ] []
      ]
  in
  describe "find by tag"
  [ describe "when there is an element with the tag"
    [ test "it finds the first element" <|
      \() ->
        case Node.findNode html "div" of
          Just node ->
            Matchers.hasId "root" node
          Nothing ->
            Expect.fail "Nothing found"
    , test "it finds a nested element" <|
      \() ->
        case Node.findNode html "input" of
          Just node ->
            Matchers.hasClass "inputField" node
          Nothing ->
            Expect.fail "Nothing found"
    ]
  ]

findByAttribute =
  let
    html = Html.div
      [ Attr.class "withAttribute"
      , Attr.attribute "data-attribute-name" "myFunAttributeValue"
      ]
      [ Html.div
        [ Attr.class "anotherWithAttribute"
        , Attr.attribute "data-attribute-name" "myDifferentAttributeValue"
        ] []
      , Html.p
        [ Attr.class "thirdWithAttribute"
        , Attr.attribute "data-attribute-name" "thirdAttributeValue"
        ] []
      ]
  in
    describe "find by attribute"
    [ describe "when nothing is specified"
      [ test "it fails" <|
        \() ->
          case Node.findNode html "" of
            Just node ->
              Expect.fail "Should not have found anything!"
            Nothing ->
              Expect.pass
      ]
    , describe "when the selector is not parseable"
      [ test "it fails" <|
        \() ->
          case Node.findNode html "[blah='stuff'" of
            Just node ->
              Expect.fail "Should not have found anything!"
            Nothing ->
              Expect.pass
      ]
    , describe "when only an attribute is specified"
      [ test "it finds the first node with the attribute" <|
        \() ->
          case Node.findNode html "[data-attribute-name]" of
            Just node ->
              Matchers.hasClass "withAttribute" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when an attribute and value is specified"
      [ test "it finds the node with the attribute and value" <|
        \() ->
          case Node.findNode html "[data-attribute-name='myDifferentAttributeValue']" of
            Just node ->
              Matchers.hasClass "anotherWithAttribute" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag and attribute is specified"
      [ test "it finds the node with the tag and attribute" <|
        \() ->
          case Node.findNode html "p[data-attribute-name]" of
            Just node ->
              Matchers.hasClass "thirdWithAttribute" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag, attribute, and value is specified"
      [ test "it finds the node with the tag and attribute and value" <|
        \() ->
          case Node.findNode html "div[data-attribute-name='myDifferentAttributeValue']" of
            Just node ->
              Matchers.hasClass "anotherWithAttribute" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    ]

findByProperty =
  let
    html = Html.div [ Attr.id "my-form" ]
      [ Html.input [ Attr.id "name-field", Attr.name "name" ] []
      , Html.input [ Attr.id "telephone-field", Attr.name "telephone" ] []
      ]
  in
    describe "find by property"
    [ describe "when only a property is specified"
      [ test "it finds the node with the property" <|
        \() ->
          case Node.findNode html "[name]" of
            Just node ->
              Matchers.hasId "name-field" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a property and value is specified"
      [ test "it finds the node with the property and value" <|
        \() ->
          case Node.findNode html "[name='telephone']" of
            Just node ->
              Matchers.hasId "telephone-field" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag and attribute is specified"
      [ test "it finds the node with the tag and attribute" <|
        \() ->
          case Node.findNode html "input[name]" of
            Just node ->
              Matchers.hasId "name-field" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag, attribute, and value is specified"
      [ test "it finds the node with the tag and attribute and value" <|
        \() ->
          case Node.findNode html "input[name='telephone']" of
            Just node ->
              Matchers.hasId "telephone-field" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    ]

classListTests : Test
classListTests =
  describe "classList"
  [ describe "when the node has no classes"
    [ test "it returns an empty array" <|
      \() ->
        Node.classList (emptyNode "div")
          |> Expect.equal []
    ]
  , describe "when the node has classes"
    [ test "it returns an array of classes" <|
      \() ->
        Node.classList (nodeWithClass "title")
          |> Expect.equal [ "title", "funClass" ]
    ]
  ]

idTests : Test
idTests =
  describe "id"
  [ describe "when the node has no id"
    [ test "it retuns Nothing" <|
      \() ->
        Node.id (emptyNode "div")
          |> Expect.equal Nothing
    ]
  , describe "when the node has an id"
    [ test "it returns the id" <|
      \() ->
        Node.id (nodeWithId "fun")
          |> Expect.equal (Just "fun")
    ]
  ]

propertyTests : Test
propertyTests =
  describe "property"
  [ describe "when the node has no properties" <|
    [ test "it returns nothing" <|
      \() ->
        Node.property "innerHTML" (emptyNode "div")
          |> Expect.equal Nothing
    ]
  , describe "when the node has properties"
    [ describe "when the node does not have the requested property"
      [ test "it returns nothing" <|
        \() ->
          Node.property "innerHTML" (nodeWithProperty ("blah", "blah"))
            |> Expect.equal Nothing
      ]
    , describe "when the node has the requested property"
      [ test "it returns the property" <|
        \() ->
          Node.property "innerHTML" (nodeWithProperty ("innerHTML", "blah"))
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
        nodeResult = Native.Helpers.asHtmlNode sampleHtml
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
            Expect.equal (Node.toString node) expected
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

findChildrenTests : Test
findChildrenTests =
  describe "findChildren"
  [ describe "when the node has no matching children"
    [ test "it fails" <|
      \() ->
        Node.findChildren ".some-class" (emptyNode "div")
          |> Expect.equal []
    ]
  , describe "when the node has matching children"
    [ test "it finds the children" <|
      \() ->
        Node.findChildren "li" nodeWithList
          |> Expect.equal [(emptyNode "li"), (emptyNode "li"), (emptyNode "li")]
    ]
  ]
