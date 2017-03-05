module Elmer.InputEventTests exposing (all)

import Test exposing (..)
import Expect
import Elmer
import Elmer.TestApps.InputTestApp as App
import Elmer.EventTests as EventTests
import Elmer.Internal exposing (..)
import Elmer.Html.Event as Event
import Elmer.Html as Markup
import Elmer.Printer exposing (..)

all : Test
all =
  describe "Input Event Tests"
    [ inputTests
    , checkTests
    , uncheckTests
    , submitTests
    , selectTests
    ]


inputTests =
  describe "input event tests"
  [ EventTests.standardEventBehavior (Event.input "fun stuff") "input"
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
  [ EventTests.standardEventBehavior Event.check "change"
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
  [ EventTests.standardEventBehavior Event.uncheck "change"
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

triggersSubmit : String -> Test
triggersSubmit selector =
  describe "submittable behavior"
  [ describe "when there is no submit handler on an ancestor"
    [ test "it returns an error" <|
      \() ->
        let
          state = Elmer.componentState App.defaultModel App.submitWithoutFormView App.update
            |> Markup.find selector
            |> Event.click
        in
          case state of
            Ready component ->
              Expect.fail "Should fail!"
            Failed msg ->
              Expect.equal msg "No relevant event handler found"
    ]
  , let
      initialModel = App.defaultModel
      state = Elmer.componentState initialModel App.view App.update
        |> Markup.find selector
        |> Event.click
    in
      describe "when there is a submit handler on an ancestor"
      [ test "at first no submit is recorded" <|
        \() ->
          Expect.equal initialModel.isSubmitted False
      , test "it handles the event" <|
        \() ->
          case state of
            Ready component ->
              Expect.equal component.model.isSubmitted True
            Failed msg ->
              Expect.fail msg
      ]
  , let
      initialModel = App.defaultModel
      state = Elmer.componentState initialModel App.submitOutsideFormView App.update
        |> Markup.find selector
        |> Event.click
    in
      describe "when the submit handler is on a form referenced by the submit button"
      [ test "at first no submit is recorded" <|
        \() ->
          Expect.equal initialModel.isSubmitted False
      , test "it handles the event" <|
        \() ->
          case state of
            Ready component ->
              Expect.equal component.model.isSubmitted True
            Failed msg ->
              Expect.fail msg
      ]
  , describe "when the submit button references a form that does not exist"
    [ let
        initialModel = App.defaultModel
        state = Elmer.componentState initialModel App.submitBadFormDescendentView App.update
          |> Markup.find selector
          |> Event.click
      in
        describe "when the targeted element is not the descendent of a form"
        [ test "at first no submit is recorded" <|
          \() ->
            Expect.equal initialModel.isSubmitted False
        , test "it does nothing" <|
          \() ->
            case state of
              Ready component ->
                Expect.fail "Should fail!"
              Failed msg ->
                Expect.equal msg "No relevant event handler found"
        ]
      , let
          initialModel = App.defaultModel
          state = Elmer.componentState initialModel App.submitBadFormView App.update
            |> Markup.find selector
            |> Event.click
        in
          describe "when the targeted element is the descendent of a form with a submit handler"
          [ test "at first no submit is recorded" <|
            \() ->
              Expect.equal initialModel.isSubmitted False
          , test "it does nothing" <|
            \() ->
              case state of
                Ready component ->
                  Expect.fail "Should fail!"
                Failed msg ->
                  Expect.equal msg "No relevant event handler found"
          ]
      ]
  ]

doesNotTriggerSubmit : String -> Test
doesNotTriggerSubmit selector =
  let
      initialModel = App.defaultModel
      state = Elmer.componentState initialModel App.view App.update
          |> Markup.find selector
          |> Event.click
    in
      describe "when there is a submit handler on an ancestor"
      [ test "at first no submit is recorded" <|
        \() ->
          Expect.equal initialModel.isSubmitted False
      , test "it does not trigger a submit event" <|
        \() ->
          case state of
            Ready component ->
              Expect.equal component.model.isSubmitted False
            Failed msg ->
              Expect.fail msg
      ]

submitTests : Test
submitTests =
  describe "submit event"
  [ describe "input with type submit"
    [ triggersSubmit "input[type='submit']"
    ]
  , describe "input with type other than submit"
    [ doesNotTriggerSubmit "input[type='text']"
    ]
  , describe "button with submit type"
    [ triggersSubmit "button[type='submit']"
    ]
  , describe "button with no type"
    [ triggersSubmit "#default-type-button"
    ]
  , describe "button with type other than submit"
    [ doesNotTriggerSubmit "button[type='button']"
    ]
  ]

selectTests : Test
selectTests =
  describe "select"
  [ describe "when there is an upstream failure"
    [ test "it passes on the error" <|
      \() ->
        let
          initialState = Failed "upstream failure"
        in
          Event.select "some-value" initialState
            |> Expect.equal initialState
    ]
  , describe "when there is no target node"
    [ test "it returns an upstream failure" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Event.select "some-value" initialState
           |> Expect.equal (Failed "No target element specified")
    ]
  , describe "when the element is not a select"
    [ test "it fails" <|
      \() ->
        let
          state = Elmer.componentState App.defaultModel App.view App.update
            |> Markup.find "#root"
            |> Event.select "some-value"
        in
          Expect.equal state (Failed "The targeted element is not selectable")
    ]
  , describe "when the element is a select"
    [ describe "when no input event handler is found"
      [ test "it fails" <|
        \() ->
          let
            state = Elmer.componentState App.defaultModel App.selectWithNoHandlerView App.update
              |> Markup.find "select"
              |> Event.select "some-value"
          in
            Expect.equal state (Failed "No relevant event handler found")
      ]
    , describe "when the select has no options"
      [ test "it fails" <|
        \() ->
          let
            state = Elmer.componentState App.defaultModel App.selectWithNoOptionsView App.update
              |> Markup.find "select"
              |> Event.select "some-value"
          in
            Expect.equal state (Failed (format [ message "No option found with value" "some-value" ]))
      ]
    , describe "when the select has options"
      [ describe "when no option matches the specified value"
        [ test "it fails" <|
          \() ->
            let
              state = Elmer.componentState App.defaultModel App.selectView App.update
                |> Markup.find "select"
                |> Event.select "bad-value"
            in
              Expect.equal state (
                Failed <| format
                  [ message "No option found with value" "bad-value"
                  , message "These are the options" "- select  [ input ]\n  - option { value = 'cat' } \n    - Cat\n  - option { value = 'dog' } \n    - Dog\n  - option { value = 'mouse' } \n    - Mouse"
                  ]
              )
        ]
      , describe "when an option matches the specified value"
        [ test "it triggers the event handler" <|
          \() ->
            let
              state = Elmer.componentState App.defaultModel App.selectView App.update
                |> Markup.find "select"
                |> Event.select "mouse"
            in
              case state of
                Ready component ->
                  Expect.equal component.model.selectedValue "mouse"
                Failed msg ->
                  Expect.fail msg
        ]
      ]
    ]
  ]
