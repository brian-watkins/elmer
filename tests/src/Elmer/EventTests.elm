module Elmer.EventTests exposing (..)

import Test exposing (..)
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Elmer.TestApps.InputTestApp as InputApp
import Elmer.TestApps.EventPropagationTestApp as EventApp
import Expect
import Elmer
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Html.Event as Event
import Elmer.Html.Selector as Sel
import Elmer.TestHelpers exposing (printHtml)
import Elmer.Errors as Errors
import Elmer.Platform.Command as Command
import Elmer.Html as Markup


all : Test
all =
  Test.concat
  [ customEventTests
  ]

standardEventBehavior : String -> (TestState SimpleApp.Model SimpleApp.Msg -> TestState SimpleApp.Model SimpleApp.Msg) -> Test
standardEventBehavior eventTypes eventFunction =
  describe "Event Behavior"
  [ describe "when there is an upstream failure"
    [ test "it passes on the error" <|
      \() ->
        let
          initialState = TestState.failure "upstream failure"
        in
          eventFunction initialState
            |> Expect.equal initialState
    ]
  , describe "when there is no target node"
    [ test "it returns an upstream failure" <|
      \() ->
        let
          initialState = Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
        in
          eventFunction initialState
           |> Expect.equal (TestState.failure "No element has been targeted. Use Elmer.Html.target to identify an element to receive the event.")
    ]
  , describe "when the targeted element is not found"
    [ test "it returns a failure" <|
      \() ->
        Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          |> Markup.target << Sel.by [ Sel.id "nothing" ]
          |> eventFunction
          |> Expect.equal (TestState.failure <|
            Errors.print <| Errors.elementNotFound <| printHtml (SimpleApp.view SimpleApp.defaultModel)
          )
    ]
  , describe "when the event handler is not found"
    [ test "it returns an event not found error" <|
      \() ->
        Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          |> Markup.target << Sel.by [ Sel.class "no-events" ]
          |> eventFunction
          |> Expect.equal (TestState.failure ("No event handlers found for any of the triggered events: " ++ eventTypes))
    ]
  ]

multiEventPropagationBehavior : Int -> Int -> (TestState EventApp.Model EventApp.Msg -> TestState EventApp.Model EventApp.Msg) -> String -> Test
multiEventPropagationBehavior allEvents nonPropagatingEvents eventFunc eventName =
  describe (eventName ++ " event propagation tests")
  [ describe "when there is no event handler on the target element"
    [ test "the event bubbles up through all the ancestors" <|
      \() ->
        let
          state = Elmer.given EventApp.defaultModel EventApp.view EventApp.update
                    |> Markup.target << Sel.by [ Sel.id "no-events" ]
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
          state = Elmer.given EventApp.defaultModel (EventApp.viewWithNonPropagatingEvent eventName) EventApp.update
                    |> Markup.target << Sel.by [ Sel.id "no-events" ]
                    |> eventFunc
        in
          Elmer.expectModel (\model ->
            Expect.equal model.eventCount nonPropagatingEvents
          ) state
    ]
  ]

propagationBehavior : (TestState EventApp.Model EventApp.Msg -> TestState EventApp.Model EventApp.Msg) -> String -> Test
propagationBehavior =
  multiEventPropagationBehavior 3 2

customEventTests =
  let
    keyUpEventJson = "{\"keyCode\":65}"
  in
    describe "custom event tests"
    [ standardEventBehavior "keyup" (Event.trigger "keyup" keyUpEventJson)
    , describe "when the event succeeds"
      [ test "it updates the model accordingly" <|
        \() ->
          Elmer.given InputApp.defaultModel InputApp.view InputApp.update
            |> Markup.target << Sel.by [ Sel.tag "input", Sel.characteristic ( "name", Just "first-name" ) ]
            |> Event.trigger "keyup" keyUpEventJson
            |> Elmer.expectModel (\model ->
              Expect.equal model.lastLetter 65
            )
      ]
    ]
