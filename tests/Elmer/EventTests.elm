module Elmer.EventTests exposing (..)

import Test exposing (..)
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Elmer.TestApps.InputTestApp as InputApp
import Elmer.TestApps.EventPropagationTestApp as EventApp
import Expect
import Elmer
import Elmer.ComponentState as ComponentState exposing (ComponentState)
import Elmer.Html.Event as Event
import Elmer.Platform.Command as Command
import Elmer.Html as Markup


standardEventBehavior : (ComponentState SimpleApp.Model SimpleApp.Msg -> ComponentState SimpleApp.Model SimpleApp.Msg) -> Test
standardEventBehavior eventFunction =
  describe "Event Behavior"
  [ describe "when there is an upstream failure"
    [ test "it passes on the error" <|
      \() ->
        let
          initialState = ComponentState.failure "upstream failure"
        in
          eventFunction initialState
            |> Expect.equal initialState
    ]
  , describe "when there is no target node"
    [ test "it returns an upstream failure" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
        in
          eventFunction initialState
           |> Expect.equal (ComponentState.failure "No element has been targeted. Use Elmer.Html.target to identify an element to receive the event.")
    ]
  , describe "when the targeted element is not found"
    [ test "it returns a failure" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
        in
          Markup.target "#nothing" initialState
            |> eventFunction
            |> Expect.equal (ComponentState.failure
              "No html element found with selector: #nothing\n\nThe current view is:\n\n- div { className = 'styled no-events', id = 'root' } \n  - Some text"
            )
    ]
  , describe "when the event handler is not found"
    [ test "it returns an event not found error" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
        in
          Markup.target ".no-events" initialState
            |> eventFunction
            |> Expect.equal (ComponentState.failure ("No relevant event handler found"))
    ]
  ]

multiEventPropagationBehavior : Int -> Int -> (ComponentState EventApp.Model EventApp.Msg -> ComponentState EventApp.Model EventApp.Msg) -> String -> Test
multiEventPropagationBehavior allEvents nonPropagatingEvents eventFunc eventName =
  describe (eventName ++ " event propagation tests")
  [ describe "when there is no event handler on the target element"
    [ test "the event bubbles up through all the ancestors" <|
      \() ->
        let
          state = Elmer.componentState EventApp.defaultModel EventApp.view EventApp.update
                    |> Markup.target "#no-events"
                    |> eventFunc
        in
          Elmer.expectModel (\model ->
            Expect.equal model.eventCount allEvents
          ) state
    ]
  , describe "when an event handler has stopPropagation set to True"
    [ test "the event stops at the non-propagating event handler" <|
      \() ->
        let
          state = Elmer.componentState EventApp.defaultModel (EventApp.viewWithNonPropagatingEvent eventName) EventApp.update
                    |> Markup.target "#no-events"
                    |> eventFunc
        in
          Elmer.expectModel (\model ->
            Expect.equal model.eventCount nonPropagatingEvents
          ) state
    ]
  ]

propagationBehavior : (ComponentState EventApp.Model EventApp.Msg -> ComponentState EventApp.Model EventApp.Msg) -> String -> Test
propagationBehavior =
  multiEventPropagationBehavior 3 2

customEventTests =
  let
    keyUpEventJson = "{\"keyCode\":65}"
  in
    describe "custom event tests"
    [ standardEventBehavior (Event.trigger "keyup" keyUpEventJson)
    , describe "when the event succeeds"
      [ test "it updates the model accordingly" <|
        \() ->
          let
            initialState = Elmer.componentState InputApp.defaultModel InputApp.view InputApp.update
            updatedStateResult = Markup.target "input[name='first-name']" initialState
                                  |> Event.trigger "keyup" keyUpEventJson
          in
            updatedStateResult
              |> Elmer.expectModel (\model ->
                Expect.equal model.lastLetter 65
              )
      ]
    ]
