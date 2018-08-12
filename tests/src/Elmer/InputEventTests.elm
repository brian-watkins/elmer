module Elmer.InputEventTests exposing (..)

import Test exposing (..)
import Expect
import Elmer
import Elmer.TestApps.InputTestApp as App
import Elmer.EventTests as EventTests
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Html.Event as Event
import Elmer.Html as Markup
import Elmer.Spy as Spy exposing (andCallFake)
import Elmer.Spy.Matchers exposing (wasCalled)
import Elmer.Printer exposing (..)

all : Test
all =
  Test.concat
  [ inputTests
  , checkTests
  , uncheckTests
  , submitTests
  , submitWithSpyTests
  , selectTests
  ]

inputTests =
  describe "input event tests"
  [ EventTests.standardEventBehavior "input" (Event.input "fun stuff")
  , EventTests.multiEventPropagationBehavior 1 1 (Event.input "fun stuff") "input" -- Note that Elm forces input events to have stopPropagation set to True
  , describe "when the input succeeds"
    [ test "it updates the model accordingly" <|
      \() ->
        Elmer.given App.defaultModel App.view App.update
          |> Markup.target "input[name='first-name']"
          |> Event.input "Mr. Fun Stuff"
          |> Elmer.expectModel (\model ->
              Expect.equal model.name "Mr. Fun Stuff"
            )
    ]
  ]


checkTests : Test
checkTests =
  describe "check event"
  [ EventTests.standardEventBehavior "change" Event.check
  , EventTests.propagationBehavior Event.check "change"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.view App.update
    in
      describe "the check event"
      [ test "at first no check is recorded" <|
        \() ->
          Expect.equal initialModel.isChecked False
      , test "the event updates the model" <|
        \() ->
          Markup.target "input[name='is-cool']" initialState
            |> Event.check
            |> Elmer.expectModel (\model ->
                Expect.equal model.isChecked True
              )
      ]
  ]


uncheckTests : Test
uncheckTests =
  describe "uncheck event"
  [ EventTests.standardEventBehavior "change" Event.uncheck
  , EventTests.propagationBehavior Event.uncheck "change"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.view App.update
    in
      describe "the uncheck event"
      [ test "at first no check is recorded" <|
        \() ->
          Expect.equal initialModel.isChecked False
      , test "the event updates the model" <|
        \() ->
          Markup.target "input[name='is-cool']" initialState
            |> Event.check
            |> Event.uncheck
            |> Elmer.expectModel (\model ->
                Expect.equal model.isChecked False
              )
      ]
  ]

triggersSubmit : String -> Test
triggersSubmit selector =
  describe "submittable behavior"
  [ describe "when there is no submit handler on an ancestor"
    [ test "it returns an error" <|
      \() ->
        let
          state = Elmer.given App.defaultModel App.submitWithoutFormView App.update
            |> Markup.target selector
            |> Event.click
        in
          Expect.equal state <| TestState.failure "No event handlers found for any of the triggered events: click, mousedown, mouseup, submit"
    ]
  , let
      initialModel = App.defaultModel
      state = Elmer.given initialModel App.view App.update
        |> Markup.target selector
        |> Event.click
    in
      describe "when there is a submit handler on an ancestor"
      [ test "at first no submit is recorded" <|
        \() ->
          Expect.equal initialModel.isSubmitted False
      , test "it handles the event" <|
        \() ->
          state
            |> Elmer.expectModel (\model ->
                Expect.equal model.isSubmitted True
              )
      ]
  , let
      initialModel = App.defaultModel
      state = Elmer.given initialModel App.submitOutsideFormView App.update
        |> Markup.target selector
        |> Event.click
    in
      describe "when the submit handler is on a form referenced by the submit button"
      [ test "at first no submit is recorded" <|
        \() ->
          Expect.equal initialModel.isSubmitted False
      , test "it handles the event" <|
        \() ->
          state
            |> Elmer.expectModel (\model ->
                Expect.equal model.isSubmitted True
              )
      ]
  , describe "when the submit button references a form that does not exist"
    [ let
        initialModel = App.defaultModel
        state = Elmer.given initialModel App.submitBadFormDescendentView App.update
          |> Markup.target selector
          |> Event.click
      in
        describe "when the targeted element is not the descendent of a form"
        [ test "at first no submit is recorded" <|
          \() ->
            Expect.equal initialModel.isSubmitted False
        , test "it does nothing" <|
          \() ->
            Expect.equal state <| TestState.failure "No event handlers found for any of the triggered events: click, mousedown, mouseup, submit"
        ]
      , let
          initialModel = App.defaultModel
          state = Elmer.given initialModel App.submitBadFormView App.update
            |> Markup.target selector
            |> Event.click
        in
          describe "when the targeted element is the descendent of a form with a submit handler"
          [ test "at first no submit is recorded" <|
            \() ->
              Expect.equal initialModel.isSubmitted False
          , test "it does nothing" <|
            \() ->
              Expect.equal state <| TestState.failure "No event handlers found for any of the triggered events: click, mousedown, mouseup, submit"
          ]
      ]
  ]

doesNotTriggerSubmit : String -> Test
doesNotTriggerSubmit selector =
  let
      initialModel = App.defaultModel
      state = Elmer.given initialModel App.view App.update
          |> Markup.target selector
          |> Event.click
    in
      describe "when there is a submit handler on an ancestor"
      [ test "at first no submit is recorded" <|
        \() ->
          Expect.equal initialModel.isSubmitted False
      , test "it does not trigger a submit event" <|
        \() ->
          state
            |> Elmer.expectModel (\model ->
                Expect.equal model.isSubmitted False
              )
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

submitWithSpyTests : Test
submitWithSpyTests =
  describe "submit with a view spy"
  [ describe "when the view is faked by a spy"
    [ test "it does the right thing" <|
      \() ->
        let
          viewSpy =
            Spy.create "outside-form-spy" (\_ -> App.submitBadFormDescendentView)
              |> andCallFake (\model -> App.submitOutsideFormView model)
        in
          Elmer.given App.defaultModel App.spyTestView App.update
            |> Spy.use [ viewSpy ]
            |> Markup.target "#default-type-button"
            |> Event.click
            |> Elmer.expectModel (\model ->
                Expect.equal model.isSubmitted True
              )
    ]
  ]

selectTests : Test
selectTests =
  describe "select"
  [ describe "when there is an upstream failure"
    [ test "it passes on the error" <|
      \() ->
        let
          initialState = TestState.failure "upstream failure"
        in
          Event.select "some-value" initialState
            |> Expect.equal initialState
    ]
  , describe "when there is no target node"
    [ test "it returns an upstream failure" <|
      \() ->
        let
          initialState = Elmer.given App.defaultModel App.view App.update
        in
          Event.select "some-value" initialState
           |> Expect.equal (TestState.failure "No element has been targeted. Use Elmer.Html.target to identify an element to receive the event.")
    ]
  , describe "when the element is not a select"
    [ test "it fails" <|
      \() ->
        let
          state = Elmer.given App.defaultModel App.view App.update
            |> Markup.target "#root"
            |> Event.select "some-value"
        in
          Expect.equal state (TestState.failure "The targeted element is not selectable")
    ]
  , describe "when the element is a select"
    [ describe "when no input event handler is found"
      [ test "it fails" <|
        \() ->
          let
            state = Elmer.given App.defaultModel App.selectWithNoHandlerView App.update
              |> Markup.target "select"
              |> Event.select "some-value"
          in
            Expect.equal state (TestState.failure "No event handlers found for any of the triggered events: input")
      ]
    , describe "when the select has no options"
      [ test "it fails" <|
        \() ->
          let
            state = Elmer.given App.defaultModel App.selectWithNoOptionsView App.update
              |> Markup.target "select"
              |> Event.select "some-value"
          in
            Expect.equal state (TestState.failure (format [ message "No option found with value" "some-value" ]))
      ]
    , describe "when the select has options"
      [ describe "when no option matches the specified value"
        [ test "it fails" <|
          \() ->
            let
              state = Elmer.given App.defaultModel App.selectView App.update
                |> Markup.target "select"
                |> Event.select "bad-value"
            in
              Expect.equal state (
                TestState.failure <| format
                  [ message "No option found with value" "bad-value"
                  , message "These are the options" "- select  [ input ]\n  - option { value = 'cat' } \n    - Cat\n  - option { value = 'dog' } \n    - Dog\n  - option { value = 'mouse' } \n    - Mouse"
                  ]
              )
        ]
      , describe "when an option matches the specified value"
        [ test "it triggers the event handler" <|
          \() ->
            Elmer.given App.defaultModel App.selectView App.update
              |> Markup.target "select"
              |> Event.select "mouse"
              |> Elmer.expectModel (\model ->
                  Expect.equal model.selectedValue "mouse"
                )
        ]
      ]
    ]
  ]
