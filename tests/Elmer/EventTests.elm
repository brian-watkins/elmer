module Elmer.EventTests exposing
  ( all
  , standardEventHandlerBehavior
  , propagationBehavior
  )

import Test exposing (..)
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Elmer.TestApps.InputTestApp as InputApp
import Elmer.TestApps.EventPropagationTestApp as EventApp
import Expect
import Elmer
import Elmer.Internal exposing (..)
import Elmer.Html.Event as Event
import Elmer.Platform.Command as Command
import Elmer.Html as Markup

all : Test
all =
  describe "Event Tests"
    [ customEventTests
    ]

standardEventHandlerBehavior : (ComponentState SimpleApp.Model SimpleApp.Msg -> ComponentState SimpleApp.Model SimpleApp.Msg) -> String -> Test
standardEventHandlerBehavior eventHandler eventName =
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
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
        in
          eventHandler initialState
           |> Expect.equal (Failed "No target element specified")
    ]
  , describe "when the event is not found on the target node"
    [ test "it returns an event not found error" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
        in
          Markup.find ".no-events" initialState
            |> eventHandler
            |> Expect.equal (Failed ("No relevant event handler found"))
    ]
  ]

propagationBehavior : (ComponentState EventApp.Model EventApp.Msg -> ComponentState EventApp.Model EventApp.Msg) -> String -> Test
propagationBehavior eventFunc eventName =
  describe "event propagation tests"
  [ describe "when there is no event handler on the target element"
    [ test "the event bubbles up through all the ancestors" <|
      \() ->
        let
          state = Elmer.componentState EventApp.defaultModel EventApp.view EventApp.update
                    |> Markup.find "#no-events"
                    |> eventFunc
        in
          case state of
            Ready s ->
              Expect.equal s.model.eventCount 3
            Failed msg ->
              Expect.fail msg
    ]
  , describe "when an event handler has stopPropagation set to True"
    [ test "the event stops at the non-propagating event handler" <|
      \() ->
        let
          state = Elmer.componentState EventApp.defaultModel (EventApp.viewWithNonPropagatingEvent eventName) EventApp.update
                    |> Markup.find "#no-events"
                    |> eventFunc
        in
          case state of
            Ready s ->
              Expect.equal s.model.eventCount 2
            Failed msg ->
              Expect.fail msg
    ]
  ]

customEventTests =
  let
    keyUpEventJson = "{\"keyCode\":65}"
  in
    describe "custom event tests"
    [ standardEventHandlerBehavior (Event.on "keyup" keyUpEventJson) "keyup"
    , describe "when the event succeeds"
      [ test "it updates the model accordingly" <|
        \() ->
          let
            initialState = Elmer.componentState InputApp.defaultModel InputApp.view InputApp.update
            updatedStateResult = Markup.find "input[name='first-name']" initialState
                                  |> Event.trigger "keyup" keyUpEventJson
          in
            case updatedStateResult of
              Ready updatedState ->
                Expect.equal updatedState.model.lastLetter 65
              Failed msg ->
                Expect.fail msg
      ]
    ]
