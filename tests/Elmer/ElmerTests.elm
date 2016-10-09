module Elmer.ElmerTests exposing (all)

import Test exposing (..)
import Elmer.TestApp as App
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer.Types exposing (..)
import Elmer.Event.Input as InputEvent
import Elmer
import Elmer.Matchers as Matchers

all : Test
all =
  describe "Elmer Tests"
    [ noElementFound
    , findById
    , findByClass
    , findTests
    , expectNodeTests
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
            Expect.equal ( Elmer.findResult html "#nothing" ) (SearchFailure "No html node found with selector: #nothing")
      ]
    , describe "with bad class"
      [ test "it returns nothing" <|
        \() ->
          let
            html = App.view App.defaultModel
          in
            Expect.equal ( Elmer.findResult html ".nothing" ) (SearchFailure "No html node found with selector: .nothing")
      ]
    , describe "when there is only text"
      [ test "it returns nothing" <|
        \() ->
          let
            html = App.onlyText
          in
            Expect.equal ( Elmer.findResult html ".anything" ) (SearchFailure "No html node found with selector: .anything")
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
                  case (Elmer.findResult html "#root") of
                    Found a ->
                      Expect.equal a.id (Just "root")
                    SearchFailure msg ->
                      Expect.fail msg
        , test "finds a nested element by id" <|
            \() ->
                let
                  html = App.view App.defaultModel
                in
                  case (Elmer.findResult html "#userNameLabel") of
                    Found a ->
                      Expect.equal a.id (Just "userNameLabel")
                    SearchFailure msg ->
                      Expect.fail msg
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
          case ( Elmer.findResult html ".content" ) of
            Found a ->
              Matchers.hasClass "content" a
            SearchFailure msg ->
              Expect.fail msg
      , test "it finds a nested element by class" <|
        \() ->
          case ( Elmer.findResult html ".label" ) of
            Found a ->
              Matchers.hasClass "label" a
            SearchFailure msg ->
              Expect.fail msg
      ]
    , describe "when there is more than one class"
      [ test "it finds the element" <|
        \() ->
          case ( Elmer.findResult html ".awesome" ) of
            Found a ->
              Matchers.hasClass "awesome" a
            SearchFailure msg ->
              Expect.fail msg
      ]
    , describe "when the class name is the same as an id"
      [ test "it returns the element with the class name" <|
        \() ->
          case ( Elmer.findResult html ".root" ) of
            Found a ->
              Matchers.hasClass "root" a
            SearchFailure msg ->
              Expect.fail msg
      ]
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
    [ test "it returns the failure message" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Elmer.find ".blah" initialState
            |> Expect.equal (UpstreamFailure "No html node found with selector: .blah")
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
            |> Expect.equal (Expect.fail "No target node specified")
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
