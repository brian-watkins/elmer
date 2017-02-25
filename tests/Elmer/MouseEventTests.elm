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
