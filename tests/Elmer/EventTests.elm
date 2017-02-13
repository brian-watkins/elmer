module Elmer.EventTests exposing (all)

import Test exposing (..)
import Elmer.TestApps.ClickTestApp as ClickApp
import Elmer.TestApps.InputTestApp as InputApp
import Elmer.TestApps.MessageTestApp as MessageApp
import Expect
import Elmer
import Elmer.Internal exposing (..)
import Elmer.Html.Event as Event
import Elmer.Command as Command
import Elmer.Html as Markup

all : Test
all =
  describe "Event Tests"
    [ clickTests
    , inputTests
    , customEventTests
    ]

standardEventHandlerBehavior : (ComponentState ClickApp.Model ClickApp.Msg -> ComponentState ClickApp.Model ClickApp.Msg) -> String -> Test
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
          initialState = Elmer.componentState ClickApp.defaultModel ClickApp.view ClickApp.update
        in
          eventHandler initialState
           |> Expect.equal (Failed "No target node specified")
    ]
  , describe "when the event is not found on the target node"
    [ test "it returns an event not found error" <|
      \() ->
        let
          initialState = Elmer.componentState ClickApp.defaultModel ClickApp.view ClickApp.update
        in
          Markup.find ".noEvents" initialState
            |> eventHandler
            |> Expect.equal (Failed ("No " ++ eventName ++ " event found"))
    ]
  ]

clickTests =
  describe "Click Event Tests"
  [ standardEventHandlerBehavior Event.click "click"
  , describe "when the click succeeds"
    [ test "it updates the model accordingly" <|
      \() ->
        let
          initialState = Elmer.componentState ClickApp.defaultModel ClickApp.view ClickApp.update
          updatedStateResult = Markup.find ".button" initialState
                                |> Event.click
        in
          case updatedStateResult of
            Ready updatedState ->
              Expect.equal updatedState.model.clicks 1
            Failed msg ->
              Expect.fail msg
    ]
  ]

inputTests =
  describe "input event tests"
  [ standardEventHandlerBehavior (Event.input "fun stuff") "input"
  , describe "when the input succeeds"
    [ test "it updates the model accordingly" <|
      \() ->
        let
          initialState = Elmer.componentState InputApp.defaultModel InputApp.view InputApp.update
          updatedStateResult = Markup.find ".nameField" initialState
                                |> Event.input "Mr. Fun Stuff"
        in
          case updatedStateResult of
            Ready updatedState ->
              Expect.equal updatedState.model.name "Mr. Fun Stuff"
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
            updatedStateResult = Markup.find ".nameField" initialState
                                  |> Event.on "keyup" keyUpEventJson
          in
            case updatedStateResult of
              Ready updatedState ->
                Expect.equal updatedState.model.lastLetter 65
              Failed msg ->
                Expect.fail msg
      ]
    ]
