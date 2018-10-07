module Elmer.HtmlQueryTests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Elmer
import Elmer.Html.Matchers as Matchers exposing (element, elements)
import Elmer.Html.Node as Node
import Elmer.Html.Element as Element
import Elmer.Html exposing (..)
import Elmer.Html.Selector as S exposing (..)
import Elmer.Html.Types exposing (HtmlSelectorGroup)
import Elmer.TestState as TestState exposing (TestState)
import Elmer.TestHelpers exposing (..)
import Html.Attributes as Attr
import Html exposing (Html)


all : Test
all =
  Test.concat
    [ noElementFound
    , findById
    , findByClass
    , findByTag
    , findByAttribute
    , findByProperty
    , descendantTests
    , findDescendantsTests
    , findWithAll
    , customSelectorTest
    , findByText
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
            initialState html
              |> target << by [ id "nothing" ]
              |> expect (Elmer.expectNot <| Matchers.elementExists)
      ]
    , describe "with bad class"
      [ test "it returns nothing" <|
        \() ->
          let
            html = Html.div [ Attr.class "something" ] []
          in
            initialState html
              |> target << by [ class "nothing" ]
              |> expect (Elmer.expectNot <| Matchers.elementExists)
      ]
    , describe "when there is only text"
      [ test "it returns nothing" <|
        \() ->
          let
            html = Html.text "Something"
          in
            initialState html
              |> target << by [ class "anything" ]
              |> expect (Elmer.expectNot <| Matchers.elementExists)
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
          initialState html
            |> target << by [ id "root" ]
            |> expect (element <| Matchers.hasId "root")
      , test "finds a nested element by id" <|
        \() ->
          initialState html
            |> target << by [ id "nested" ]
            |> expect (element <| Matchers.hasId "nested")
      ]

findByClass : Test
findByClass =
  describe "find by class"
    [ let
        html = Html.div [ Attr.class "content_class_2" ]
          [ Html.div [ Attr.class "nested-class" ] []
          ]
      in
        describe "when there is one class"
        [ test "it finds the top element by class" <|
          \() ->
            initialState html
              |> target << by [ class "content_class_2" ]
              |> expect (element <| Matchers.hasClass "content_class_2")
        , test "it finds a nested element by class" <|
          \() ->
            initialState html
              |> target << by [ class "nested-class" ]
              |> expect (element <| Matchers.hasClass "nested-class")
        ]
    , let
        html = Html.div [ Attr.classList [ ("awesome", True), ("super", True), ("root", True) ] ] []
      in
        describe "when there is more than one class"
        [ test "it finds the element" <|
          \() ->
            initialState html
              |> target << by [ class "super" ]
              |> expect (element <| Matchers.hasClass "super")
        ]
    , let
        html = Html.div [ Attr.id "root", Attr.class "root" ] []
      in
        describe "when the class name is the same as an id"
        [ test "it returns the element with the class name" <|
          \() ->
            initialState html
              |> target << by [ class "root" ]
              |> expect (element <| Matchers.hasClass "root")
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
            initialState html
              |> target << by [ class "deeplyNested" ]
              |> expect (element <| Matchers.hasClass "deeplyNested")
        ]
    ]

findByTag =
  describe "find by tag"
  [ describe "when there is an element with the tag" <|
    let
      html = Html.div [ Attr.id "root" ]
        [ Html.input [ Attr.class "inputField" ] []
        ]
    in
    [ test "it finds the first element" <|
      \() ->
        initialState html
          |> target << by [ tag "div" ]
          |> expect (element <| Matchers.hasId "root")
    , test "it finds a nested element" <|
      \() ->
        initialState html
          |> target << by [ tag "input" ]
          |> expect (element <| Matchers.hasClass "inputField")
    ]
  , describe "when the tag is qualified by a class" <|
    let
      html =
        Html.div [ Attr.id "root" ]
        [ Html.p [ Attr.class "title" ] [ Html.text "the title" ]
        , Html.p [ Attr.class "description_2" ] [ Html.text "the description" ]
        ]
    in
    [ test "it finds the tag with the class" <|
      \() ->
        initialState html
          |> target << by [ tag "p", class "description_2" ]
          |> expect (element <| Matchers.hasText "the description")
    ]
  ]


findByText : Test
findByText =
  describe "find by text" <|
    let
      html =
        Html.div []
        [ Html.div []
          [ Html.text "One"
          , Html.div [ Attr.class "first" ]
            [ Html.text "Three"
            ]
          ]
        , Html.div [ Attr.class "second" ]
          [ Html.text "Two"
          , Html.text "Three"
          ]
        ]
    in
      [ describe "when the element has the text"
        [ test "it finds the element" <|
          \() ->
            initialState html
              |> target << by [ text "Three" ]
              |> expect (elements <| Elmer.expectAll
                [ Elmer.hasLength 2
                , Elmer.atIndex 0 <| Matchers.hasText "Three"
                , Elmer.atIndex 1 <| Matchers.hasText "Three"
                , Elmer.some <| Matchers.hasClass "first"
                , Elmer.some <| Matchers.hasClass "second"
                ]
              )
        , describe "when there is on element with the text"
          [ test "it finds no elements" <|
            \() ->
              initialState html
                |> target << by [ text "nothing" ]
                |> expect (Elmer.expectNot <| Matchers.elementExists)
          ]
        ]
      ]


findByAttribute =
  let
    html = Html.div
      [ Attr.class "withAttribute"
      , Attr.attribute "data-attribute_name-1" "myFunAttributeValue"
      ]
      [ Html.div
        [ Attr.class "anotherWithAttribute"
        , Attr.attribute "data-attribute_name-1" "myDifferent-Attribute_Value2"
        ] []
      , Html.p
        [ Attr.class "thirdWithAttribute"
        , Attr.attribute "data-attribute_name-1" "thirdAttributeValue"
        ] []
      ]
  in
    describe "find by attribute"
    [ describe "when nothing is specified"
      [ test "it fails" <|
        \() ->
          initialState html
            |> target << by []
            |> expect (Elmer.expectNot <| Matchers.elementExists)
      ]
    , describe "when only an attribute is specified"
      [ test "it finds the first node with the attribute" <|
        \() ->
          initialState html
            |> target << by [ characteristic ("data-attribute_name-1", Nothing) ]
            |> expect (element <| Matchers.hasClass "withAttribute")
      ]
    , describe "when an attribute and value is specified"
      [ test "it finds the node with the attribute and value" <|
        \() ->
          initialState html
            |> target << by [ characteristic ("data-attribute_name-1", Just "myDifferent-Attribute_Value2") ]
            |> expect (element <| Matchers.hasClass "anotherWithAttribute")
      ]
    , describe "when a tag and attribute is specified"
      [ test "it finds the node with the tag and attribute" <|
        \() ->
          initialState html
            |> target << by [ tag "p", characteristic ("data-attribute_name-1", Nothing) ]
            |> expect (element <| Matchers.hasClass "thirdWithAttribute")
      ]
    , describe "when a tag, attribute, and value is specified"
      [ test "it finds the node with the tag and attribute and value" <|
        \() ->
          initialState html
            |> target << by [ tag "div", characteristic ("data-attribute_name-1", Just "myDifferent-Attribute_Value2") ]
            |> expect (element <| Matchers.hasClass "anotherWithAttribute")
      ]
    , describe "when an attribute and class is specified"
      [ test "it finds the element with the attribute and the class" <|
        \() ->
          initialState html
            |> target << by [ characteristic ("data-attribute_name-1", Nothing), class "anotherWithAttribute" ]
            |> expect (element <| 
              Matchers.hasAttribute ("data-attribute_name-1", "myDifferent-Attribute_Value2")
            )
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
          initialState html
            |> target << by [ characteristic ("name", Nothing) ]
            |> expect (element <| Matchers.hasId "name-field")
      ]
    , describe "when a property and value is specified"
      [ test "it finds the node with the property and value" <|
        \() ->
          initialState html
            |> target << by [ characteristic ("name", Just "telephone") ]
            |> expect (element <| Matchers.hasId "telephone-field")
      ]
    , describe "when a tag and attribute is specified"
      [ test "it finds the node with the tag and attribute" <|
        \() ->
          initialState html
            |> target << by [ tag "input", characteristic ("name", Nothing) ]
            |> expect (element <| Matchers.hasId "name-field")
      ]
    , describe "when a tag, attribute, and value is specified"
      [ test "it finds the node with the tag and attribute and value" <|
        \() ->
          initialState html
            |> target << by [ tag "input", characteristic ("name", Just "telephone") ]
            |> expect (element <| Matchers.hasId "telephone-field")
      ]
    ]

findWithAll : Test
findWithAll =
  let
      html =
        Html.div [ Attr.id "my-form" ]
        [ Html.input [ Attr.id "telephone-field", Attr.name "telephone" ]
          [ Html.div [ Attr.class "funny"] []
          ]
        , Html.input [ Attr.id "name-field", Attr.class "funny", Attr.name "name" ] []
        ]
  in
    describe "when multiple selectors are provided"
    [ test "it finds only the element with all the selectors" <|
      \() ->
        initialState html
          |> target << by [ tag "input", class "funny" ]
          |> expect (elements <| Elmer.expectAll
            [ Elmer.hasLength 1
            , Elmer.atIndex 0 <| (\element -> Expect.equal "input" <| Element.tag element)
            ]
          )
    ]


descendantTests : Test
descendantTests =
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
  describe "when an element is targeted within another"
  [ describe "when all the selectors match"
    [ test "it finds the element" <|
      \() ->
        initialState html
          |> target << descendantsOf [ tag "li", characteristic ("data-item", Just "2") ] << by [ class "author" ]
          |> expect (Matchers.element <| Elmer.expectAll 
            [ Matchers.hasClass "author"
            , Matchers.hasText "Some awesome person"
            ]
          )
    ]
  , describe "when one selector fails"
    [ test "it fails to find the element" <|
      \() ->
        initialState html
          |> target << descendantsOf [ tag "li", characteristic ("data-item", Just "99") ] << by [ class "author" ]
          |> expect (Elmer.expectNot <| Matchers.elementExists)
    ]
  ]

initialState : Html () -> TestState () ()
initialState html =
  Elmer.given () (\_ -> html) (\_ _ -> ((), Cmd.none))

getElement : Html msg -> HtmlElement msg
getElement html =
  Node.from html
    |> Node.asElement
    |> Maybe.withDefault nodeWithList

liWithDiv : String -> HtmlElement msg
liWithDiv name =
  Html.li [] [ Html.div [ Attr.class name ] [] ]
    |> getElement

divWithClass : String -> HtmlElement msg
divWithClass name =
  Html.div [ Attr.class name ] []
    |> getElement


findDescendantsTests : Test
findDescendantsTests =
  let
    html =
      Html.ul [ Attr.class "fun" ]
        [ Html.li [] [ Html.div [ Attr.class "fun" ] [] ]
        , Html.li [] [ Html.div [ Attr.class "awesome" ] [] ]
        , Html.li [] [ Html.div [ Attr.class "fun" ] [] ]
        ]
  in
  describe "findChildren"
  [ describe "when the node has no matching children"
    [ test "it fails" <|
      \() ->
        initialState html
          |> target << by [ class "some-class" ]
          |> expect (elements <| Expect.equal [])
    ]
  , describe "when the node has matching children"
    [ test "it finds the children" <|
      \() ->
        initialState html
          |> target << by [ tag "li" ]
          |> expect (elements <| 
            Expect.equal [liWithDiv "fun", liWithDiv "awesome", liWithDiv "fun"]
          )
    ]
  , describe "when finding descendants"
    [ test "it finds the descendants" <|
      \() ->
        initialState html
          |> target << descendantsOf [ tag "li" ] << by [ class "fun" ]
          |> expect (elements <|
            Expect.equal [ divWithClass "fun", divWithClass "fun" ]
          )
    ]
  ]


customSelectorTest : Test
customSelectorTest =
  describe "custom selector" <|
    let
      html =
        Html.div [ Attr.class "funny" ]
        [ Html.ul [ Attr.class "list-fun" ]
          [ Html.li [] [ Html.div [ Attr.class "fun" ] [] ]
          , Html.li [] [ Html.div [ Attr.class "awesome" ] [] ]
          , Html.li [] [ Html.div [ Attr.class "fun" ] [] ]
          ]
        , Html.div [ Attr.class "super" ] []
        ]
    in
      [ test "it matches with the custom selector" <|
        \() ->
          initialState html
            |> target << descendantsOf [ tag "ul" ] << by [ testOrClassSelector [ "super", "awesome", "funny" ] ]
            |> expect (element <|
                Matchers.hasClass "awesome"
            )
      ]


testOrClassSelector : List String -> HtmlSelector msg
testOrClassSelector expectedClasses element =
  Element.classList element
    |> List.filter (\c -> List.member c expectedClasses)
    |> not << List.isEmpty
