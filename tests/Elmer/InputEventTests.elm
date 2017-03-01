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
  , EventTests.propagationBehavior (Event.input "fun stuff") "input"
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


checkTests : Test
checkTests =
  describe "check event"
  [ EventTests.standardEventHandlerBehavior Event.check "change"
  , EventTests.propagationBehavior Event.check "change"
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
  [ EventTests.standardEventHandlerBehavior Event.uncheck "change"
  , EventTests.propagationBehavior Event.uncheck "change"
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
