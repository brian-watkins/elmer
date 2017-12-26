module Elmer.HtmlQueryTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.Html exposing (HtmlElement)
import Elmer.Html.Query as Query
import Elmer.Html.Matchers as Matchers
import Elmer.Html.Node as Node
import Elmer.TestHelpers exposing (..)
import Html.Attributes as Attr
import Html exposing (Html)



noElementFound : Test
noElementFound =
  describe "when no element found"
    [ describe "with bad id"
      [ test "it returns a failure mesage" <|
        \() ->
          let
            html = Html.div [ Attr.id "something" ] []
          in
            Expect.equal ( Query.forHtml "#nothing" html |> Query.findElement |> Result.toMaybe ) Nothing
      ]
    , describe "with bad class"
      [ test "it returns nothing" <|
        \() ->
          let
            html = Html.div [ Attr.class "something" ] []
          in
            Expect.equal ( Query.forHtml ".nothing" html |> Query.findElement |> Result.toMaybe ) Nothing
      ]
    , describe "when there is only text"
      [ test "it returns nothing" <|
        \() ->
          let
            html = Html.text "Something"
          in
            Expect.equal ( Query.forHtml ".anything" html |> Query.findElement |> Result.toMaybe ) Nothing
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
          case (Query.findElement <| Query.forHtml "#root" html) of
            Ok a ->
              Matchers.hasId "root" a
            Err _ ->
              Expect.fail "Nothing found"
      , test "finds a nested element by id" <|
        \() ->
          case (Query.findElement <| Query.forHtml "#nested" html) of
            Ok a ->
              Matchers.hasId "nested" a
            Err _ ->
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
            case ( Query.findElement <| Query.forHtml ".content" html ) of
              Ok a ->
                Matchers.hasClass "content" a
              Err _ ->
                Expect.fail "Nothing found"
        , test "it finds a nested element by class" <|
          \() ->
            case ( Query.findElement <| Query.forHtml ".nested" html ) of
              Ok a ->
                Matchers.hasClass "nested" a
              Err _ ->
                Expect.fail "Nothing found"
        ]
    , let
        html = Html.div [ Attr.classList [ ("awesome", True), ("super", True), ("root", True) ] ] []
      in
        describe "when there is more than one class"
        [ test "it finds the element" <|
          \() ->
            case ( Query.findElement <| Query.forHtml ".super" html ) of
              Ok a ->
                Matchers.hasClass "super" a
              Err _ ->
                Expect.fail "Nothing found"
        ]
    , let
        html = Html.div [ Attr.id "root", Attr.class "root" ] []
      in
        describe "when the class name is the same as an id"
        [ test "it returns the element with the class name" <|
          \() ->
            case ( Query.findElement <| Query.forHtml ".root" html ) of
              Ok a ->
                Matchers.hasClass "root" a
              Err _ ->
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
            case ( Query.findElement <| Query.forHtml ".deeplyNested" html ) of
              Ok a ->
                Matchers.hasClass "deeplyNested" a
              Err _ ->
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
        case Query.findElement <| Query.forHtml "div" html of
          Ok node ->
            Matchers.hasId "root" node
          Err _ ->
            Expect.fail "Nothing found"
    , test "it finds a nested element" <|
      \() ->
        case Query.findElement <| Query.forHtml "input" html of
          Ok node ->
            Matchers.hasClass "inputField" node
          Err _ ->
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
          case Query.findElement <| Query.forHtml "" html of
            Ok node ->
              Expect.fail "Should not have found anything!"
            Err _ ->
              Expect.pass
      ]
    , describe "when the selector is not parseable"
      [ test "it fails" <|
        \() ->
          case Query.findElement <| Query.forHtml "[blah='stuff'" html of
            Ok node ->
              Expect.fail "Should not have found anything!"
            Err _ ->
              Expect.pass
      ]
    , describe "when only an attribute is specified"
      [ test "it finds the first node with the attribute" <|
        \() ->
          case Query.findElement <| Query.forHtml "[data-attribute-name]" html of
            Ok node ->
              Matchers.hasClass "withAttribute" node
            Err _ ->
              Expect.fail "Nothing found"
      ]
    , describe "when an attribute and value is specified"
      [ test "it finds the node with the attribute and value" <|
        \() ->
          case Query.findElement <| Query.forHtml "[data-attribute-name='myDifferentAttributeValue']" html of
            Ok node ->
              Matchers.hasClass "anotherWithAttribute" node
            Err _ ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag and attribute is specified"
      [ test "it finds the node with the tag and attribute" <|
        \() ->
          case Query.findElement <| Query.forHtml "p[data-attribute-name]" html of
            Ok node ->
              Matchers.hasClass "thirdWithAttribute" node
            Err _ ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag, attribute, and value is specified"
      [ test "it finds the node with the tag and attribute and value" <|
        \() ->
          case Query.findElement <| Query.forHtml "div[data-attribute-name='myDifferentAttributeValue']" html of
            Ok node ->
              Matchers.hasClass "anotherWithAttribute" node
            Err _ ->
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
          case Query.findElement <| Query.forHtml "[name]" html of
            Ok node ->
              Matchers.hasId "name-field" node
            Err _ ->
              Expect.fail "Nothing found"
      ]
    , describe "when a property and value is specified"
      [ test "it finds the node with the property and value" <|
        \() ->
          case Query.findElement <| Query.forHtml "[name='telephone']" html of
            Ok node ->
              Matchers.hasId "telephone-field" node
            Err _ ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag and attribute is specified"
      [ test "it finds the node with the tag and attribute" <|
        \() ->
          case Query.findElement <| Query.forHtml "input[name]" html of
            Ok node ->
              Matchers.hasId "name-field" node
            Err _ ->
              Expect.fail "Nothing found"
      ]
    , describe "when a tag, attribute, and value is specified"
      [ test "it finds the node with the tag and attribute and value" <|
        \() ->
          case Query.findElement <| Query.forHtml "input[name='telephone']" html of
            Ok node ->
              Matchers.hasId "telephone-field" node
            Err _ ->
              Expect.fail "Nothing found"
      ]
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
  describe "when there are multiple selectors separated by spaces"
  [ describe "when all the selectors match"
    [ test "it finds the element" <|
      \() ->
        case Query.findElement <| Query.forHtml "li[data-item='2'] .author" html of
          Ok element ->
            Matchers.hasClass "author" element
          Err _ ->
            Expect.fail "Nothing found"
    ]
  , describe "when one selector fails"
    [ test "it fails to find the element" <|
      \() ->
        case Query.findElement <| Query.forHtml "li[data-item='99'] .author" html of
          Ok _ ->
            Expect.fail "Should not find anything!"
          Err _ ->
            Expect.pass
    ]
  ]

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

-- NOTE: Are these tests still necessary?
findDescendantsTests : Test
findDescendantsTests =
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
        Query.forElement ".some-class" element
          |> Query.findElements
          |> Expect.equal []
    ]
  , describe "when the node has matching children"
    [ test "it finds the children" <|
      \() ->
        Query.forElement "li" element
          |> Query.findElements
          |> Expect.equal [liWithDiv "fun", liWithDiv "awesome", liWithDiv "fun"]
    ]
  , describe "when finding descendants"
    [ test "it finds the descendants" <|
      \() ->
        Query.forElement "li .fun" element
          |> Query.findElements
          |> Expect.equal [ divWithClass "fun", divWithClass "fun" ]
    ]
  ]
