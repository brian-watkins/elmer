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
    , mouseDownTests
    , mouseUpTests
    , mouseEnterTests
    , mouseLeaveTests
    , mouseOverTests
    ]

clickTests =
  describe "Click Event Tests"
  [ EventTests.standardEventHandlerBehavior Event.click "click"
  , describe "when the click succeeds"
    [ test "it updates the model accordingly" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
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

doubleClickTests =
  describe "Double Click Event Tests"
  [ EventTests.standardEventHandlerBehavior Event.doubleClick "dblclick"
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

mouseDownTests =
  describe "Mouse Down Event Tests"
  [ EventTests.standardEventHandlerBehavior Event.mouseDown "mousedown"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.view App.update
    in
      describe "the mouse down event"
      [ test "at first no mouse down is recorded" <|
        \() ->
          Expect.equal initialModel.mouseDowns 0
      , test "the event updates the model" <|
        \() ->
          let
            updatedStateResult = Markup.find ".button" initialState
                                  |> Event.mouseDown
          in
            case updatedStateResult of
              Ready updatedState ->
                Expect.equal updatedState.model.mouseDowns 1
              Failed msg ->
                Expect.fail msg
      ]
  ]

mouseUpTests =
  describe "Mouse Up Event Tests"
  [ EventTests.standardEventHandlerBehavior Event.mouseUp "mouseup"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.view App.update
    in
      describe "the mouse up event"
      [ test "at first no mouse up is recorded" <|
        \() ->
          Expect.equal initialModel.mouseUps 0
      , test "the event updates the model" <|
        \() ->
          let
            updatedStateResult = Markup.find ".button" initialState
                                  |> Event.mouseUp
          in
            case updatedStateResult of
              Ready updatedState ->
                Expect.equal updatedState.model.mouseUps 1
              Failed msg ->
                Expect.fail msg
      ]
  ]

mouseEnterTests =
  describe "Mouse Enter Event Tests"
  [ EventTests.standardEventHandlerBehavior Event.mouseEnter "mouseenter"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.view App.update
    in
      describe "the mouse enter event"
      [ test "at first no mouse enter is recorded" <|
        \() ->
          Expect.equal initialModel.mouseEnters 0
      , test "the event updates the model" <|
        \() ->
          let
            updatedStateResult = Markup.find ".button" initialState
                                  |> Event.mouseEnter
          in
            case updatedStateResult of
              Ready updatedState ->
                Expect.equal updatedState.model.mouseEnters 1
              Failed msg ->
                Expect.fail msg
      ]
  ]

mouseLeaveTests =
  describe "Mouse Leave Event Tests"
  [ EventTests.standardEventHandlerBehavior Event.mouseLeave "mouseleave"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.view App.update
    in
      describe "the mouse leave event"
      [ test "at first no mouse leave is recorded" <|
        \() ->
          Expect.equal initialModel.mouseLeaves 0
      , test "the event updates the model" <|
        \() ->
          let
            updatedStateResult = Markup.find ".button" initialState
                                  |> Event.mouseLeave
          in
            case updatedStateResult of
              Ready updatedState ->
                Expect.equal updatedState.model.mouseLeaves 1
              Failed msg ->
                Expect.fail msg
      ]
  ]

mouseOverTests =
  describe "Mouse Over Event Tests"
  [ describe "when there is an upstream failure"
    [ test "it passes on the error" <|
      \() ->
        let
          initialState = Failed "upstream failure"
        in
          Event.mouseOver initialState
            |> Expect.equal initialState
    ]
  , describe "when there is no target node"
    [ test "it returns an upstream failure" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Event.mouseOver initialState
           |> Expect.equal (Failed "No target node specified")
    ]
  , describe "when neither the targeted node nor the ancestor registers a mouseOver event"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.viewForMouseOver App.update
        in
          Markup.find ".no-events" initialState
            |> Event.mouseOver
            |> Expect.equal (Failed ("No mouseover event found on the targeted element or its ancestors"))
    ]
  , let
      initialModel = App.defaultModel
      initialState = Elmer.componentState initialModel App.viewForMouseOver App.update
    in
      describe "when the mouseOver event is registered"
      [ describe "when the targeted element has the mouseOver event"
        [ test "at first no mouse over is recorded" <|
          \() ->
            Expect.equal initialModel.mouseOvers 0
        , test "the event updates the model" <|
          \() ->
            let
              updatedStateResult = Markup.find "#event-parent" initialState
                                    |> Event.mouseOver
            in
              case updatedStateResult of
                Ready updatedState ->
                  Expect.equal updatedState.model.mouseOvers 1
                Failed msg ->
                  Expect.fail msg
        ]
      , describe "when an ancestor of the targeted element has the mouseOver event"
        [ test "at first no mouse over is recorded" <|
          \() ->
            Expect.equal initialModel.mouseOvers 0
        , test "the event updates the model" <|
          \() ->
            let
              updatedStateResult = Markup.find "li[data-option='2']" initialState
                                    |> Event.mouseOver
                                    |> Markup.find "li[data-option='3']"
                                    |> Event.mouseOver
            in
              case updatedStateResult of
                Ready updatedState ->
                  Expect.equal updatedState.model.mouseOvers 2
                Failed msg ->
                  Expect.fail msg
        ]
      ]
  ]
