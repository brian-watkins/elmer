module Elmer.MouseEventTests exposing (all)

import Test exposing (..)
import Elmer.TestApps.MouseTestApp as App
import Elmer.EventTests as EventTests
import Expect
import Elmer
import Elmer.Internal exposing (..)
import Elmer.Html.Event as Event
import Elmer.Platform.Command as Command
import Elmer.Html as Markup

all : Test
all =
  describe "Event Tests"
    [ clickTests
    , doubleClickTests
    , pressTests
    , releaseTests
    , moveMouseInTests
    , moveMouseOutTests
    , mouseEnterTests
    , mouseLeaveTests
    , mouseOverTests
    , mouseOutTests
    ]

andExpect : Expect.Expectation -> Expect.Expectation -> Expect.Expectation
andExpect leftResult rightResult =
  if rightResult == Expect.pass then
    leftResult
  else
    rightResult

clickTests =
  describe "Click Event Tests"
  [ EventTests.standardEventBehavior Event.click
  , EventTests.multiEventPropagationBehavior 9 6 Event.click "click"
  , EventTests.multiEventPropagationBehavior 9 6 Event.click "mousedown"
  , EventTests.multiEventPropagationBehavior 9 6 Event.click "mouseup"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.view App.update
      updatedStateResult = Markup.find ".button" initialState
                            |> Event.click
    in
      describe "when the click succeeds"
      [ test "it records a click" <|
        \() ->
          case updatedStateResult of
            Ready updatedState ->
              Expect.equal initialModel.clicks 0
                |> andExpect (Expect.equal updatedState.model.clicks 1)
            Failed msg ->
              Expect.fail msg
      , test "it records a mouse down" <|
        \() ->
          case updatedStateResult of
            Ready updatedState ->
              Expect.equal initialModel.mouseDowns 0
                |> andExpect (Expect.equal updatedState.model.mouseDowns 1)
            Failed msg ->
              Expect.fail msg
      , test "it records a mouse up" <|
        \() ->
          case updatedStateResult of
            Ready updatedState ->
              Expect.equal initialModel.mouseUps 0
                |> andExpect (Expect.equal updatedState.model.mouseUps 1)
            Failed msg ->
              Expect.fail msg
      ]
  ]

doubleClickTests =
  describe "Double Click Event Tests"
  [ EventTests.standardEventBehavior Event.doubleClick
  , EventTests.propagationBehavior Event.doubleClick "dblclick"
  , describe "when the double click succeeds"
    [ test "it updates the model accordingly" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
          updatedStateResult = Markup.find ".button" initialState
                                |> Event.doubleClick
        in
          case updatedStateResult of
            Ready updatedState ->
              Expect.equal updatedState.model.doubleClicks 1
            Failed msg ->
              Expect.fail msg
    ]
  ]

pressTests =
  describe "Press Event Tests"
  [ EventTests.standardEventBehavior Event.press
  , EventTests.propagationBehavior Event.press "mousedown"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.view App.update
    in
      describe "the press event"
      [ test "at first no mouse down is recorded" <|
        \() ->
          Expect.equal initialModel.mouseDowns 0
      , test "the event updates the model" <|
        \() ->
          let
            updatedStateResult = Markup.find ".button" initialState
                                  |> Event.press
          in
            case updatedStateResult of
              Ready updatedState ->
                Expect.equal updatedState.model.mouseDowns 1
              Failed msg ->
                Expect.fail msg
      ]
  ]

releaseTests =
  describe "Release Event Tests"
  [ EventTests.standardEventBehavior Event.release
  , EventTests.propagationBehavior Event.release "mouseup"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.view App.update
    in
      describe "the release event"
      [ test "at first no mouse up is recorded" <|
        \() ->
          Expect.equal initialModel.mouseUps 0
      , test "the event updates the model" <|
        \() ->
          let
            updatedStateResult = Markup.find ".button" initialState
                                  |> Event.release
          in
            case updatedStateResult of
              Ready updatedState ->
                Expect.equal updatedState.model.mouseUps 1
              Failed msg ->
                Expect.fail msg
      ]
  ]

moveMouseInTests : Test
moveMouseInTests =
  describe "moveMouseIn"
  [ EventTests.standardEventBehavior Event.moveMouseIn
  , EventTests.propagationBehavior Event.moveMouseIn "mouseover"
  ]

moveMouseOutTests : Test
moveMouseOutTests =
  describe "moveMouseOut"
  [ EventTests.standardEventBehavior Event.moveMouseOut
  , EventTests.propagationBehavior Event.moveMouseOut "mouseout"
  ]

mouseEnterTests =
  describe "Mouse Enter Event Handler Tests"
  [ let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.viewForMouseEnterLeave App.update
    in
      describe "the mouse move in event"
      [ describe "when the element does not have a mouse enter event handler but its ancestor does"
        [ test "it fails to find the event" <|
          \() ->
            Markup.find "li[data-option='2']" initialState
              |> Event.moveMouseIn
              |> Expect.equal (Failed ("No relevant event handler found"))
        ]
      , describe "when the element has a mouse enter event handler"
        [ test "at first no mouse enter is recorded" <|
          \() ->
            Expect.equal initialModel.mouseEnters 0
        , test "the event updates the model" <|
          \() ->
            let
              updatedStateResult = Markup.find "#event-parent" initialState
                                    |> Event.moveMouseIn
            in
              case updatedStateResult of
                Ready updatedState ->
                  Expect.equal updatedState.model.mouseEnters 1
                Failed msg ->
                  Expect.fail msg
        ]
      , describe "when the element and its ancestor have a mouse enter event handler"
        [ test "it triggers only the handler on the element" <|
          \() ->
            let
              updatedStateResult = Markup.find "li[data-option='1']" initialState
                                    |> Event.moveMouseIn
            in
              case updatedStateResult of
                Ready updatedState ->
                  Expect.equal updatedState.model.mouseEnters 1
                Failed msg ->
                  Expect.fail msg
        ]
      ]
  ]

mouseLeaveTests =
  describe "Mouse Leave Event Handler Tests"
  [ let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.viewForMouseEnterLeave App.update
    in
      describe "the mouseMoveOut event"
      [ describe "when the element does not have a mouse leave event handler but its ancestor does"
        [ test "it fails to find the event" <|
          \() ->
            Markup.find "li[data-option='2']" initialState
              |> Event.moveMouseOut
              |> Expect.equal (Failed ("No relevant event handler found"))
        ]
      , describe "when the element has the mouse leave event handler"
        [ test "at first no mouse leave is recorded" <|
          \() ->
            Expect.equal initialModel.mouseLeaves 0
        , test "the event updates the model" <|
          \() ->
            let
              updatedStateResult = Markup.find "#event-parent" initialState
                                    |> Event.moveMouseOut
            in
              case updatedStateResult of
                Ready updatedState ->
                  Expect.equal updatedState.model.mouseLeaves 1
                Failed msg ->
                  Expect.fail msg
        ]
      , describe "when the element and its ancestor have a mouse leave event handler"
        [ test "it triggers only the handler on the element" <|
          \() ->
            let
              updatedStateResult = Markup.find "li[data-option='1']" initialState
                                    |> Event.moveMouseOut
            in
              case updatedStateResult of
                Ready updatedState ->
                  Expect.equal updatedState.model.mouseLeaves 1
                Failed msg ->
                  Expect.fail msg
        ]
      ]

  ]

mouseOverTests =
  describe "Mouse Over Event Handler Tests"
  [ let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.view App.update
    in
      describe "when the mouseOver event handler is registered"
      [ describe "when the targeted element has the mouseOver event handler"
        [ test "at first no mouse over is recorded" <|
          \() ->
            Expect.equal initialModel.mouseOvers 0
        , test "the event updates the model" <|
          \() ->
            let
              updatedStateResult = Markup.find ".button" initialState
                                    |> Event.moveMouseIn
            in
              case updatedStateResult of
                Ready updatedState ->
                  Expect.equal updatedState.model.mouseOvers 1
                Failed msg ->
                  Expect.fail msg
        ]
      ]
  ]

mouseOutTests =
  describe "Mouse Out Event Handler Tests"
  [ let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.view App.update
    in
      describe "when the mouseOut event handler is registered"
      [ describe "when the targeted element has the mouseOut event handler"
        [ test "at first no mouse out is recorded" <|
          \() ->
            Expect.equal initialModel.mouseOuts 0
        , test "the event updates the model" <|
          \() ->
            let
              updatedStateResult = Markup.find ".button" initialState
                                    |> Event.moveMouseOut
            in
              case updatedStateResult of
                Ready updatedState ->
                  Expect.equal updatedState.model.mouseOuts 1
                Failed msg ->
                  Expect.fail msg
        ]
      ]
  ]
