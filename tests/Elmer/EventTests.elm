module Elmer.EventTests exposing (all)

import Test exposing (..)
import Elmer.TestApp as App
import Expect
import Elmer.Types exposing (..)
import Elmer.Event as Event
import Elmer

all : Test
all =
  describe "Event Tests"
    [ clickTests
    , inputTests
    ]

standardEventHandlerBehavior : (ComponentStateResult App.Model App.Msg -> ComponentStateResult App.Model App.Msg) -> Test
standardEventHandlerBehavior eventHandler =
  describe "Event Handler Behavior"
  [ describe "when there is an upstream failure"
    [ test "it passes on the error" <|
      \() ->
        let
          initialState = UpstreamFailure "upstream failure"
        in
          eventHandler initialState
            |> Expect.equal initialState
    ]
  , describe "when there is no target node"
    [ test "it returns an upstream failure" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          eventHandler initialState
           |> Expect.equal (UpstreamFailure "No target node specified")
    ]
  ]

clickTests =
  describe "Click Event Tests"
  [ standardEventHandlerBehavior Event.click
  , describe "when there is a target node"
    [ describe "when the click fails"
      [ test "it returns a click event not found error" <|
        \() ->
          let
            initialState = Elmer.componentState App.defaultModel App.view App.update
          in
            Elmer.find ".awesome" initialState
              |> Event.click
              |> Expect.equal (UpstreamFailure "No click event found")
      ]
    ]
    , describe "when the click succeeds"
      [ test "it updates the model accordingly" <|
        \() ->
          let
            initialState = Elmer.componentState App.defaultModel App.view App.update
            updatedStateResult = Elmer.find ".button" initialState
                                  |> Event.click
          in
            case updatedStateResult of
              CurrentState updatedState ->
                Expect.equal updatedState.model.clicks 1
              UpstreamFailure msg ->
                Expect.fail msg
      ]
  ]

inputTests =
  describe "input event tests"
  [ standardEventHandlerBehavior (Event.input "fun stuff")
  , describe "when there is a target node"
    [ describe "when the input fails"
      [ test "it returns an input event not found error" <|
        \() ->
          let
            initialState = Elmer.componentState App.defaultModel App.view App.update
          in
            Elmer.find ".awesome" initialState
              |> Event.input "fun stuff"
              |> Expect.equal (UpstreamFailure "No input event found")
      ]
    ]
    , describe "when the input succeeds"
      [ test "it updates the model accordingly" <|
        \() ->
          let
            initialState = Elmer.componentState App.defaultModel App.view App.update
            updatedStateResult = Elmer.find ".nameField" initialState
                                  |> Event.input "Mr. Fun Stuff"
          in
            case updatedStateResult of
              CurrentState updatedState ->
                Expect.equal updatedState.model.name "Mr. Fun Stuff"
              UpstreamFailure msg ->
                Expect.fail msg
      ]
  ]
