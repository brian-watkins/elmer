module Elmer.InputEventTests exposing (all)

import Test exposing (..)
import Expect
import Elmer
import Elmer.TestApps.InputTestApp as App
import Elmer.EventTests as EventTests
import Elmer.Internal exposing (..)
import Elmer.Html.Event as Event
import Elmer.Html as Markup

all : Test
all =
  describe "Input Event Tests"
    [ inputTests
    , checkTests
    , uncheckTests
    ]


inputTests =
  describe "input event tests"
  [ EventTests.standardEventHandlerBehavior (Event.input "fun stuff") "input"
  , describe "when the input succeeds"
    [ test "it updates the model accordingly" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
          updatedStateResult = Markup.find "input[name='first-name']" initialState
                                |> Event.input "Mr. Fun Stuff"
        in
          case updatedStateResult of
            Ready updatedState ->
              Expect.equal updatedState.model.name "Mr. Fun Stuff"
            Failed msg ->
              Expect.fail msg
    ]
  ]


standardCheckEventHandlerBehavior : String -> (ComponentState App.Model App.Msg -> ComponentState App.Model App.Msg) -> String -> Test
standardCheckEventHandlerBehavior actionName eventHandler eventName =
  describe "Event Handler Behavior"
  [ describe "when there is an upstream failure"
    [ test "it passes on the error" <|
      \() ->
        let
          initialState = Failed "upstream failure"
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
           |> Expect.equal (Failed "No target node specified")
    ]
  , describe "when the targeted element is not a checkbox"
    [ test "it fails" <|
      \() ->
        Elmer.componentState App.defaultModel App.view App.update
          |> Markup.find "#root"
          |> eventHandler
          |> Expect.equal (Failed ("You tried to " ++ actionName ++ " an element that is not a checkbox. Are you sure that's what you want to do?"))
    ]
  , describe "when the event is not found on the targeted checkbox"
    [ test "it returns an event not found error" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Markup.find "input[name='no-op']" initialState
            |> eventHandler
            |> Expect.equal (Failed ("No " ++ eventName ++ " event found on the targeted element"))
    ]
  ]


checkTests : Test
checkTests =
  describe "check event"
  [ standardCheckEventHandlerBehavior "check" Event.check "change"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.view App.update
    in
      describe "the check event"
      [ test "at first no check is recorded" <|
        \() ->
          Expect.equal initialModel.isChecked False
      , test "the event updates the model" <|
        \() ->
          let
            updatedStateResult = Markup.find "input[name='is-cool']" initialState
                                  |> Event.check
          in
            case updatedStateResult of
              Ready updatedState ->
                Expect.equal updatedState.model.isChecked True
              Failed msg ->
                Expect.fail msg
      ]
  ]


uncheckTests : Test
uncheckTests =
  describe "uncheck event"
  [ standardCheckEventHandlerBehavior "uncheck" Event.uncheck "change"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.view App.update
    in
      describe "the uncheck event"
      [ test "at first no check is recorded" <|
        \() ->
          Expect.equal initialModel.isChecked False
      , test "the event updates the model" <|
        \() ->
          let
            updatedStateResult = Markup.find "input[name='is-cool']" initialState
                                  |> Event.check
                                  |> Event.uncheck
          in
            case updatedStateResult of
              Ready updatedState ->
                Expect.equal updatedState.model.isChecked False
              Failed msg ->
                Expect.fail msg
      ]
  ]
