module Elmer.NodeTests exposing (all)

import Test exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Node as Node
import Elmer.Matchers as Matchers
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Elmer.TestHelpers exposing (..)
import Elmer.TestApp as App

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
  ]

noElementFound : Test
noElementFound =
  describe "when no element found"
    [ describe "with bad id"
      [ test "it returns a failure mesage" <|
        \() ->
          let
            html = App.view App.defaultModel
          in
            Expect.equal ( Node.findNode html "#nothing" ) Nothing
      ]
    , describe "with bad class"
      [ test "it returns nothing" <|
        \() ->
          let
            html = App.view App.defaultModel
          in
            Expect.equal ( Node.findNode html ".nothing" ) Nothing
      ]
    , describe "when there is only text"
      [ test "it returns nothing" <|
        \() ->
          let
            html = App.onlyText
          in
            Expect.equal ( Node.findNode html ".anything" ) Nothing
      ]
    ]

findById : Test
findById =
    describe "find by id"
        [ test "it finds the top element by id" <|
            \() ->
                let
                  html = App.view App.defaultModel
                in
                  case (Node.findNode html "#root") of
                    Just a ->
                      Matchers.hasId "root" a
                      -- Expect.equal (Elmer.id a) (Just "root")
                    Nothing ->
                      Expect.fail "Nothing found"
        , test "finds a nested element by id" <|
            \() ->
                let
                  html = App.view App.defaultModel
                in
                  case (Node.findNode html "#userNameLabel") of
                    Just a ->
                      Matchers.hasId "userNameLabel" a
                      -- Expect.equal (Elmer.id a) (Just "userNameLabel")
                    Nothing ->
                      Expect.fail "Nothing found"
        ]

findByClass : Test
findByClass =
  let
    html = App.view App.defaultModel
  in
  describe "find by class"
    [ describe "when there is one class"
      [ test "it finds the top element by class" <|
        \() ->
          case ( Node.findNode html ".content" ) of
            Just a ->
              Matchers.hasClass "content" a
            Nothing ->
              Expect.fail "Nothing found"
      , test "it finds a nested element by class" <|
        \() ->
          case ( Node.findNode html ".label" ) of
            Just a ->
              Matchers.hasClass "label" a
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when there is more than one class"
      [ test "it finds the element" <|
        \() ->
          case ( Node.findNode html ".awesome" ) of
            Just a ->
              Matchers.hasClass "awesome" a
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when the class name is the same as an id"
      [ test "it returns the element with the class name" <|
        \() ->
          case ( Node.findNode html ".root" ) of
            Just a ->
              Matchers.hasClass "root" a
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when the node is nested"
      [ test "it returns the node with the class name" <|
        \() ->
          case ( Node.findNode html ".anotherWithText" ) of
            Just a ->
              Matchers.hasClass "anotherWithText" a
            Nothing ->
              Expect.fail "Nothing found"
      ]
    ]

findByTag =
  let
    html = App.view App.defaultModel
  in
  describe "find by tag"
  [ describe "when there is an element with the tag"
    [ test "it finds the first element" <|
      \() ->
        case Node.findNode html "div" of
          Just node ->
            Matchers.hasId "root" node
            -- Expect.equal (Elmer.id node) (Just "root")
          Nothing ->
            Expect.fail "Nothing found"
    , test "it finds a nested element" <|
      \() ->
        case Node.findNode html "input" of
          Just node ->
            Matchers.hasClass "nameField" node
          Nothing ->
            Expect.fail "Nothing found"
    ]
  ]

findByAttribute =
  let
    html = App.view App.defaultModel
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
      [ test "it finds the node with the attribute" <|
        \() ->
          case Node.findNode html "[data-special-node]" of
            Just node ->
              Matchers.hasClass "anotherWithText" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when an attribute and value is specified"
      [ test "it finds the node with the attribute and value" <|
        \() ->
          case Node.findNode html "[data-special-node='moreSpecialStuff']" of
            Just node ->
              Matchers.hasClass "specialer" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag and attribute is specified"
      [ test "it finds the node with the tag and attribute" <|
        \() ->
          case Node.findNode html "p[data-special-node]" of
            Just node ->
              Matchers.hasClass "special" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag, attribute, and value is specified"
      [ test "it finds the node with the tag and attribute and value" <|
        \() ->
          case Node.findNode html "p[data-special-node='moreSpecialStuff']" of
            Just node ->
              Matchers.hasClass "specialer" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    ]

type alias FormModel =
  { name: String
  , telephone: String
  }

initialFormModel : FormModel
initialFormModel =
  { name = "Person"
  , telephone = "919-999-9999"
  }

formView : FormModel -> Html msg
formView model =
  Html.div [ Attr.id "my-form" ]
  [ Html.input [ Attr.id "name-field", Attr.name "name" ] []
  , Html.input [ Attr.id "telephone-field", Attr.name "telephone" ] []
  ]

findByProperty =
  let
    html = formView initialFormModel
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
        nodeResult = Native.Helpers.asHtmlNode html
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

html : Html Msg
html =
  Html.div [ Attr.id "title", Attr.class "myClass" ]
  [ Html.p [ Attr.class "button", Events.onClick Click ] [ Html.text "Some text" ]
  , Html.p [ Attr.class "description", Attr.attribute "data-fun-stuff" "bowling" ] [ Html.text "More text" ]
  ]
