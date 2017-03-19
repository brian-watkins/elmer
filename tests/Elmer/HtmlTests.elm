module Elmer.HtmlTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Internal exposing (..)
import Elmer.Html as Markup
import Elmer
import Elmer.Html.Matchers as Matchers
import Elmer.Html.Query as Query
import Elmer.TestHelpers exposing (..)
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Html.Attributes as Attr
import Html exposing (Html)

all : Test
all =
  describe "Html Tests"
  [ findTests
  , findChildrenTests
  , noElementFound
  , findById
  , findByClass
  , findByTag
  , findByAttribute
  , findByProperty
  , findDescendantTests
  , expectElementTests
  , expectElementExistsTests
  , expectElementsTests
  , childNodeTests
  ]

findTests =
  describe "find based on component state"
  [ describe "when there is an upstream failure"
    [ test "it returns the failure" <|
      \() ->
        let
          initialState = Failed "upstream failure"
        in
          Markup.find ".button" initialState
            |> Expect.equal initialState
    ]
  , describe "when no element is found"
    [ describe "when there is a node" <|
      [ test "it returns the failure message and prints the view" <|
        \() ->
          let
            initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          in
            Markup.find ".blah" initialState
              |> Expect.equal (Failed "No html node found with selector: .blah\n\nThe current view is:\n\n- div { className = 'styled no-events', id = 'root' } \n  - Some text")
      ]
    , describe "when there is only text" <|
      [ test "it returns the failure message and prints that there are no nodes" <|
        \() ->
          let
            initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.textView SimpleApp.update
          in
            Markup.find ".blah" initialState
              |> Expect.equal (Failed "No html node found with selector: .blah\n\nThe current view is:\n\n<No Nodes>")
      ]
    ]
  , describe "when the element is found"
    [ test "it updates the state with the target selector" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          stateResult = Markup.find ".styled" initialState
        in
          case stateResult of
            Ready state ->
              case state.targetSelector of
                Just selector ->
                  Expect.equal ".styled" selector
                Nothing ->
                  Expect.fail "No target selector!"
            Failed message ->
              Expect.fail message
    ]
  ]

getElement : Html msg -> Markup.HtmlElement msg
getElement html =
  Maybe.withDefault nodeWithList <| Native.Html.asHtmlElement html

liWithDiv : String -> Markup.HtmlElement msg
liWithDiv name =
  Html.li [] [ Html.div [ Attr.class name ] [] ]
    |> getElement

divWithClass : String -> Markup.HtmlElement msg
divWithClass name =
  Html.div [ Attr.class name ] []
    |> getElement

findChildrenTests : Test
findChildrenTests =
  let
    html =
      Html.ul []
        [ Html.li [] [ Html.div [ Attr.class "fun" ] [] ]
        , Html.li [] [ Html.div [ Attr.class "awesome" ] [] ]
        , Html.li [] [ Html.div [ Attr.class "fun" ] [] ]
        ]
    element = getElement html
  in
  describe "findChildren"
  [ describe "when the node has no matching children"
    [ test "it fails" <|
      \() ->
        Markup.findChildren ".some-class" element
          |> Expect.equal []
    ]
  , describe "when the node has matching children"
    [ test "it finds the children" <|
      \() ->
        Markup.findChildren "li" element
          |> Expect.equal [liWithDiv "fun", liWithDiv "awesome", liWithDiv "fun"]
    ]
  , describe "when finding descendants"
    [ test "it finds the descendants" <|
      \() ->
        Markup.findChildren "li .fun" element
          |> Expect.equal [ divWithClass "fun", divWithClass "fun" ]
    ]
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
            Expect.equal ( Query.findElement "#nothing" html ) Nothing
      ]
    , describe "with bad class"
      [ test "it returns nothing" <|
        \() ->
          let
            html = Html.div [ Attr.class "something" ] []
          in
            Expect.equal ( Query.findElement ".nothing" html ) Nothing
      ]
    , describe "when there is only text"
      [ test "it returns nothing" <|
        \() ->
          let
            html = Html.text "Something"
          in
            Expect.equal ( Query.findElement ".anything" html ) Nothing
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
          case (Query.findElement "#root" html) of
            Just a ->
              Matchers.hasId "root" a
            Nothing ->
              Expect.fail "Nothing found"
      , test "finds a nested element by id" <|
        \() ->
          case (Query.findElement "#nested" html) of
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
            case ( Query.findElement ".content" html ) of
              Just a ->
                Matchers.hasClass "content" a
              Nothing ->
                Expect.fail "Nothing found"
        , test "it finds a nested element by class" <|
          \() ->
            case ( Query.findElement ".nested" html ) of
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
            case ( Query.findElement ".super" html ) of
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
            case ( Query.findElement ".root" html ) of
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
            case ( Query.findElement ".deeplyNested" html ) of
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
        case Query.findElement "div" html of
          Just node ->
            Matchers.hasId "root" node
          Nothing ->
            Expect.fail "Nothing found"
    , test "it finds a nested element" <|
      \() ->
        case Query.findElement "input" html of
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
          case Query.findElement "" html of
            Just node ->
              Expect.fail "Should not have found anything!"
            Nothing ->
              Expect.pass
      ]
    , describe "when the selector is not parseable"
      [ test "it fails" <|
        \() ->
          case Query.findElement "[blah='stuff'" html of
            Just node ->
              Expect.fail "Should not have found anything!"
            Nothing ->
              Expect.pass
      ]
    , describe "when only an attribute is specified"
      [ test "it finds the first node with the attribute" <|
        \() ->
          case Query.findElement "[data-attribute-name]" html of
            Just node ->
              Matchers.hasClass "withAttribute" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when an attribute and value is specified"
      [ test "it finds the node with the attribute and value" <|
        \() ->
          case Query.findElement "[data-attribute-name='myDifferentAttributeValue']" html of
            Just node ->
              Matchers.hasClass "anotherWithAttribute" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag and attribute is specified"
      [ test "it finds the node with the tag and attribute" <|
        \() ->
          case Query.findElement "p[data-attribute-name]" html of
            Just node ->
              Matchers.hasClass "thirdWithAttribute" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag, attribute, and value is specified"
      [ test "it finds the node with the tag and attribute and value" <|
        \() ->
          case Query.findElement "div[data-attribute-name='myDifferentAttributeValue']" html of
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
          case Query.findElement "[name]" html of
            Just node ->
              Matchers.hasId "name-field" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a property and value is specified"
      [ test "it finds the node with the property and value" <|
        \() ->
          case Query.findElement "[name='telephone']" html of
            Just node ->
              Matchers.hasId "telephone-field" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag and attribute is specified"
      [ test "it finds the node with the tag and attribute" <|
        \() ->
          case Query.findElement "input[name]" html of
            Just node ->
              Matchers.hasId "name-field" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag, attribute, and value is specified"
      [ test "it finds the node with the tag and attribute and value" <|
        \() ->
          case Query.findElement "input[name='telephone']" html of
            Just node ->
              Matchers.hasId "telephone-field" node
            Nothing ->
              Expect.fail "Nothing found"
      ]
    ]

findDescendantTests : Test
findDescendantTests =
  let
    html = Html.ul [ Attr.id "list" ]
      [ Html.li [ Attr.attribute "data-item" "1" ]
        [ Html.div [] [ Html.text "Another Item" ]
        , Html.div [ Attr.class "header" ]
          [ Html.div [ Attr.class "title" ] [ Html.text "Fun Item #1" ]
          , Html.div [ Attr.class "author" ] [ Html.text "Some fun person" ]
          ]
        , Html.div [ Attr.class "body" ] [ Html.text "Some info about the fun item." ]
        ]
      , Html.li [ Attr.attribute "data-item" "2" ]
        [ Html.div [] [ Html.text "Another Item" ]
        , Html.div [ Attr.class "header" ]
          [ Html.div [ Attr.class "title" ] [ Html.text "Awesome Item #1" ]
          , Html.div [ Attr.class "author" ] [ Html.text "Some awesome person" ]
          ]
        , Html.div [ Attr.class "body" ] [ Html.text "Some info about the awesome item." ]
        ]
      , Html.div [ Attr.class "footer" ] [ Html.text "Footer text" ]
      ]
  in
  describe "when there are multiple selectors separated by spaces"
  [ describe "when all the selectors match"
    [ test "it finds the element" <|
      \() ->
        case Query.findElement "li[data-item='2'] .author" html of
          Just element ->
            Matchers.hasClass "author" element
          Nothing ->
            Expect.fail "Nothing found"
    ]
  , describe "when one selector fails"
    [ test "it fails to find the element" <|
      \() ->
        case Query.findElement "li[data-item='99'] .author" html of
          Just _ ->
            Expect.fail "Should not find anything!"
          Nothing ->
            Expect.pass
    ]
  ]


expectElementTests =
  describe "expect node"
  [ describe "when there is an upstream failure"
    [ test "it fails with the error message" <|
      \() ->
        let
          initialState = Failed "upstream failure"
        in
          Markup.expectElement (
            \node -> Expect.fail "Should not get here"
          ) initialState
            |> Expect.equal (Expect.fail "upstream failure")
    ]
  , describe "when there is no target node"
    [ test "it fails with an error" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
        in
          Markup.expectElement (
            \node -> Expect.fail "Should not get here"
          ) initialState
            |> Expect.equal (Expect.fail "No element targeted")
    ]
  , describe "when there is a target node"
    [ test "it executes the expectation function" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
        in
          Markup.find ".styled" initialState
            |> Markup.expectElement (
                  \node -> Expect.equal "div" node.tag
                )
            |> Expect.equal Expect.pass
    ]
  ]

expectElementExistsTests =
  describe "expect node exists"
  [ describe "when there is an upstream failure"
    [ test "it fails with the upstream error message" <|
      \() ->
        let
          initialState = Failed "upstream failure"
        in
          Markup.expectElementExists initialState
            |> Expect.equal (Expect.fail "upstream failure")
    ]
  , describe "when there is no target node"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
        in
          Markup.expectElementExists initialState
            |> Expect.equal (Expect.fail "No element targeted")
    ]
  , describe "where there is a target node"
    [ test "it passes" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
        in
          Markup.find "#root" initialState
            |> Markup.expectElementExists
            |> Expect.equal Expect.pass
    ]
  ]

expectElementsTests : Test
expectElementsTests =
  describe "expectElements"
  [ describe "when there is a failure upstream"
    [ test "it fails" <|
      \() ->
        let
          initialState = Failed "You failed!"
        in
          Markup.expectElements (\elements -> Expect.fail "Should not get here!") initialState
            |> Expect.equal (Expect.fail "You failed!")
    ]
  , describe "when no target is specified"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
        in
          Markup.expectElements (
            \elements -> Expect.fail "Should not get here"
          ) initialState
            |> Expect.equal (Expect.fail "No elements targeted")
    ]
  , describe "when elements are found"
    [ test "it applies the matcher" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.viewWithChildren SimpleApp.update
        in
          Markup.find "div" initialState
            |> Markup.expectElements (
              \elements -> Expect.equal (List.length elements) 4
            )
            |> Expect.equal Expect.pass
    ]
  ]

childNodeTests =
  describe "nodes with children"
  [ describe "when there is a child node with text"
    [ test "it finds the text" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.viewWithChildren SimpleApp.update
        in
          Markup.find "#root" initialState
            |> Markup.expectElement (
                  \node ->
                    Matchers.hasText "Child text" node
                )
            |> Expect.equal Expect.pass
    ]
  ]
