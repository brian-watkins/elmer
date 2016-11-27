module Elmer.NodeTests exposing (all)

import Test exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Node as Node
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

all : Test
all =
  describe "node tests"
  [ toStringTests
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
                  ++ "  - p  \n"
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
  , Html.p [] [ Html.text "More text" ]
  ]
