module Elmer.ElmerTests exposing (all)

import Test exposing (..)
import Elmer.TestApp as App
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer.Event as Event
import Elmer exposing (..)
import Elmer.Types exposing (..)
import Elmer.Node as Node
import Elmer.Matchers as Matchers

all : Test
all =
  describe "Elmer Tests"
    [ findTests
    , expectNodeTests
    , expectNodeExistsTests
    , childNodeTests
    ]


findTests =
  describe "find based on component state"
  [ describe "when there is an upstream failure"
    [ test "it returns the failure" <|
      \() ->
        let
          initialState = UpstreamFailure "upstream failure"
        in
          Elmer.find ".button" initialState
            |> Expect.equal initialState
    ]
  , describe "when no element is found"
    [ describe "when there is a node" <|
      [ test "it returns the failure message and prints the view" <|
        \() ->
          let
            initialState = Elmer.componentState App.defaultModel App.simpleView App.update
          in
            Elmer.find ".blah" initialState
              |> Expect.equal (UpstreamFailure "No html node found with selector: .blah\n\nThe current view is:\n\n- div { id = 'root' } \n  - Some text")
      ]
    , describe "when there is only text" <|
      [ test "it returns the failure message and prints that there are no nodes" <|
        \() ->
          let
            initialState = Elmer.componentState App.defaultModel App.textView App.update
          in
            Elmer.find ".blah" initialState
              |> Expect.equal (UpstreamFailure "No html node found with selector: .blah\n\nThe current view is:\n\n<No Nodes>")
      ]
    ]
  , describe "when the element is found"
    [ test "it updates the state with the targetnode" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
          stateResult = Elmer.find ".button" initialState
        in
          case stateResult of
            CurrentState state ->
              case state.targetNode of
                Just node ->
                  Expect.equal node.tag "div"
                Nothing ->
                  Expect.fail "No target node!"
            UpstreamFailure message ->
              Expect.fail message
    ]
  ]


expectNodeTests =
  describe "expect node"
  [ describe "when there is an upstream failure"
    [ test "it fails with the error message" <|
      \() ->
        let
          initialState = UpstreamFailure "upstream failure"
        in
          Elmer.expectNode (
            \node -> Expect.fail "Should not get here"
          ) initialState
            |> Expect.equal (Expect.fail "upstream failure")
    ]
  , describe "when there is no target node"
    [ test "it fails with an error" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Elmer.expectNode (
            \node -> Expect.fail "Should not get here"
          ) initialState
            |> Expect.equal (Expect.fail "Node does not exist")
    ]
  , describe "when there is a target node"
    [ test "it executes the expectation function" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Elmer.find ".awesome" initialState
            |> Elmer.expectNode (
                  \node -> Expect.equal "div" node.tag
                )
            |> Expect.equal Expect.pass
    ]
  ]

expectNodeExistsTests =
  describe "expect node exists"
  [ describe "when there is an upstream failure"
    [ test "it fails with the upstream error message" <|
      \() ->
        let
          initialState = UpstreamFailure "upstream failure"
        in
          Elmer.expectNodeExists initialState
            |> Expect.equal (Expect.fail "upstream failure")
    ]
  , describe "when there is no target node"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Elmer.expectNodeExists initialState
            |> Expect.equal (Expect.fail "Node does not exist")
    ]
  , describe "where there is a target node"
    [ test "it passes" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Elmer.find ".awesome" initialState
            |> Elmer.expectNodeExists
            |> Expect.equal Expect.pass
    ]
  ]

childNodeTests =
  describe "nodes with children"
  [ describe "when there is a child node with text"
    [ test "it finds the text" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Elmer.find "#root" initialState
            |> Elmer.expectNode (
                  \node ->
                    Matchers.hasText "my text" node
                )
            |> Expect.equal Expect.pass
    ]
  ]
