module Elmer.MouseEventTests exposing (..)

import Test exposing (..)
import Elmer.TestApps.MouseTestApp as App
import Elmer.EventTests as EventTests
import Expect
import Elmer
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Html.Event as Event
import Elmer.Platform.Command as Command
import Elmer.Html as Markup


andExpect : Expect.Expectation -> Expect.Expectation -> Expect.Expectation
andExpect leftResult rightResult =
  if rightResult == Expect.pass then
    leftResult
  else
    rightResult

clickTests =
  describe "Click Event Tests"
  [ EventTests.standardEventBehavior "click, mousedown, mouseup, submit" Event.click
  , EventTests.multiEventPropagationBehavior 9 6 Event.click "click"
  , EventTests.multiEventPropagationBehavior 9 6 Event.click "mousedown"
  , EventTests.multiEventPropagationBehavior 9 6 Event.click "mouseup"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.view App.update
      updatedStateResult = Markup.target ".button" initialState
                            |> Event.click
    in
      describe "when the click succeeds"
      [ test "it records a click" <|
        \() ->
          updatedStateResult
            |> Elmer.expectModel (\model ->
                Expect.equal initialModel.clicks 0
                  |> andExpect (Expect.equal model.clicks 1)
              )
      , test "it records a mouse down" <|
        \() ->
          updatedStateResult
            |> Elmer.expectModel (\model ->
                Expect.equal initialModel.mouseUps 0
                  |> andExpect (Expect.equal model.mouseDowns 1)
              )
      , test "it records a mouse up" <|
        \() ->
          updatedStateResult
            |> Elmer.expectModel (\model ->
                Expect.equal initialModel.mouseUps 0
                  |> andExpect (Expect.equal model.mouseUps 1)
              )
      , test "it records a position of (0, 0)" <|
        \() ->
          Elmer.given initialModel App.viewForPosition App.update
            |> Markup.target ".button"
            |> Event.click
            |> Elmer.expectModel (\model ->
              Expect.equal initialModel.position Nothing
                |> andExpect (Expect.equal model.position <| Just { x = 0, y = 0 })
              )
      ]
  ]

doubleClickTests =
  describe "Double Click Event Tests"
  [ EventTests.standardEventBehavior "click, mousedown, mouseup, submit" Event.doubleClick
  , EventTests.multiEventPropagationBehavior 21 14 Event.doubleClick "dblclick"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.view App.update
      updatedStateResult = Markup.target ".button" initialState
                            |> Event.doubleClick
    in
      describe "when the double click succeeds"
      [ test "it records the double click" <|
        \() ->
          updatedStateResult
            |> Elmer.expectModel (\model ->
                Expect.equal initialModel.doubleClicks 0
                  |> andExpect (Expect.equal model.doubleClicks 1)
              )
      , test "it records two clicks" <|
        \() ->
          updatedStateResult
            |> Elmer.expectModel (\model ->
                Expect.equal initialModel.clicks 0
                  |> andExpect (Expect.equal model.clicks 2)
              )
      , test "it records two mouse downs" <|
        \() ->
          updatedStateResult
            |> Elmer.expectModel (\model ->
                Expect.equal initialModel.mouseDowns 0
                  |> andExpect (Expect.equal model.mouseDowns 2)
              )
      , test "it records two mouse ups" <|
        \() ->
          updatedStateResult
            |> Elmer.expectModel (\model ->
                Expect.equal initialModel.mouseUps 0
                  |> andExpect (Expect.equal model.mouseUps 2)
              )
      ]
  ]

pressTests =
  describe "Press Event Tests"
  [ EventTests.standardEventBehavior "mousedown" Event.press
  , EventTests.propagationBehavior Event.press "mousedown"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.view App.update
    in
      describe "the press event"
      [ test "at first no mouse down is recorded" <|
        \() ->
          Expect.equal initialModel.mouseDowns 0
      , test "the event updates the model" <|
        \() ->
          Markup.target ".button" initialState
            |> Event.press
            |> Elmer.expectModel (\model ->
                Expect.equal model.mouseDowns 1
              )
      , test "it records a position of (0, 0)" <|
        \() ->
          Elmer.given initialModel App.viewForPosition App.update
            |> Markup.target ".button"
            |> Event.press
            |> Elmer.expectModel (\model ->
              Expect.equal initialModel.position Nothing
                |> andExpect (Expect.equal model.position <| Just { x = 0, y = 0 })
              )
      ]
  ]

releaseTests =
  describe "Release Event Tests"
  [ EventTests.standardEventBehavior "mouseup" Event.release
  , EventTests.propagationBehavior Event.release "mouseup"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.view App.update
    in
      describe "the release event"
      [ test "at first no mouse up is recorded" <|
        \() ->
          Expect.equal initialModel.mouseUps 0
      , test "the event updates the model" <|
        \() ->
          Markup.target ".button" initialState
            |> Event.release
            |> Elmer.expectModel (\model ->
                Expect.equal model.mouseUps 1
              )
      , test "it records a position of (0, 0)" <|
        \() ->
          Elmer.given initialModel App.viewForPosition App.update
            |> Markup.target ".button"
            |> Event.release
            |> Elmer.expectModel (\model ->
              Expect.equal initialModel.position Nothing
                |> andExpect (Expect.equal model.position <| Just { x = 0, y = 0 })
              )
      ]
  ]

moveMouseInTests : Test
moveMouseInTests =
  describe "moveMouseIn"
  [ EventTests.standardEventBehavior "mouseenter, mouseover" Event.moveMouseIn
  , EventTests.propagationBehavior Event.moveMouseIn "mouseover"
  , test "it records a position of (0, 0)" <|
    \() ->
      let
        initialModel = App.defaultModel
      in
        Elmer.given initialModel App.viewForPosition App.update
          |> Markup.target ".button"
          |> Event.moveMouseIn
          |> Elmer.expectModel (\model ->
            Expect.equal initialModel.position Nothing
              |> andExpect (Expect.equal model.position <| Just { x = 0, y = 0 })
            )
  ]

moveMouseOutTests : Test
moveMouseOutTests =
  describe "moveMouseOut"
  [ EventTests.standardEventBehavior "mouseleave, mouseout" Event.moveMouseOut
  , EventTests.propagationBehavior Event.moveMouseOut "mouseout"
  , test "it records a position of (0, 0)" <|
    \() ->
      let
        initialModel = App.defaultModel
      in
        Elmer.given initialModel App.viewForPosition App.update
          |> Markup.target ".button"
          |> Event.moveMouseOut
          |> Elmer.expectModel (\model ->
            Expect.equal initialModel.position Nothing
              |> andExpect (Expect.equal model.position <| Just { x = 0, y = 0 })
            )
  ]

mouseEnterTests =
  describe "Mouse Enter Event Handler Tests"
  [ let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.viewForMouseEnterLeave App.update
    in
      describe "the mouse move in event"
      [ describe "when the element does not have a mouse enter event handler but its ancestor does"
        [ test "it fails to find the event" <|
          \() ->
            Markup.target "li[data-option='2']" initialState
              |> Event.moveMouseIn
              |> Expect.equal (TestState.failure "No event handlers found for any of the triggered events: mouseenter, mouseover")
        ]
      , describe "when the element has a mouse enter event handler"
        [ test "at first no mouse enter is recorded" <|
          \() ->
            Expect.equal initialModel.mouseEnters 0
        , test "the event updates the model" <|
          \() ->
            Markup.target "#event-parent" initialState
              |> Event.moveMouseIn
              |> Elmer.expectModel (\model ->
                  Expect.equal model.mouseEnters 1
                )
        , test "it records a position of (0, 0)" <|
          \() ->
            Elmer.given initialModel App.viewForPosition App.update
              |> Markup.target "#enter-leave-element"
              |> Event.moveMouseIn
              |> Elmer.expectModel (\model ->
                Expect.equal initialModel.position Nothing
                  |> andExpect (Expect.equal model.position <| Just { x = 0, y = 0 })
                )
        ]
      , describe "when the element and its ancestor have a mouse enter event handler"
        [ test "it triggers only the handler on the element" <|
          \() ->
            Markup.target "li[data-option='1']" initialState
              |> Event.moveMouseIn
              |> Elmer.expectModel (\model ->
                  Expect.equal model.mouseEnters 1
                )
        ]
      ]
  ]

mouseLeaveTests =
  describe "Mouse Leave Event Handler Tests"
  [ let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.viewForMouseEnterLeave App.update
    in
      describe "the mouseMoveOut event"
      [ describe "when the element does not have a mouse leave event handler but its ancestor does"
        [ test "it fails to find the event" <|
          \() ->
            Markup.target "li[data-option='2']" initialState
              |> Event.moveMouseOut
              |> Expect.equal (TestState.failure "No event handlers found for any of the triggered events: mouseleave, mouseout")
        ]
      , describe "when the element has the mouse leave event handler"
        [ test "at first no mouse leave is recorded" <|
          \() ->
            Expect.equal initialModel.mouseLeaves 0
        , test "the event updates the model" <|
          \() ->
            Markup.target "#event-parent" initialState
              |> Event.moveMouseOut
              |> Elmer.expectModel (\model ->
                  Expect.equal model.mouseLeaves 1
                )
        , test "it records a position of (0, 0)" <|
          \() ->
            Elmer.given initialModel App.viewForPosition App.update
              |> Markup.target "#enter-leave-element"
              |> Event.moveMouseOut
              |> Elmer.expectModel (\model ->
                Expect.equal initialModel.position Nothing
                  |> andExpect (Expect.equal model.position <| Just { x = 0, y = 0 })
                )
        ]
      , describe "when the element and its ancestor have a mouse leave event handler"
        [ test "it triggers only the handler on the element" <|
          \() ->
            Markup.target "li[data-option='1']" initialState
              |> Event.moveMouseOut
              |> Elmer.expectModel (\model ->
                  Expect.equal model.mouseLeaves 1
                )
        ]
      ]

  ]

mouseOverTests =
  describe "Mouse Over Event Handler Tests"
  [ let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.view App.update
    in
      describe "when the mouseOver event handler is registered"
      [ describe "when the targeted element has the mouseOver event handler"
        [ test "at first no mouse over is recorded" <|
          \() ->
            Expect.equal initialModel.mouseOvers 0
        , test "the event updates the model" <|
          \() ->
            Markup.target ".button" initialState
              |> Event.moveMouseIn
              |> Elmer.expectModel (\model ->
                  Expect.equal model.mouseOvers 1
                )
        , test "it records a position of (0, 0)" <|
          \() ->
            Elmer.given initialModel App.viewForPosition App.update
              |> Markup.target ".button"
              |> Event.moveMouseIn
              |> Elmer.expectModel (\model ->
                Expect.equal initialModel.position Nothing
                  |> andExpect (Expect.equal model.position <| Just { x = 0, y = 0 })
                )
        ]
      ]
  ]

mouseOutTests =
  describe "Mouse Out Event Handler Tests"
  [ let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.view App.update
    in
      describe "when the mouseOut event handler is registered"
      [ describe "when the targeted element has the mouseOut event handler"
        [ test "at first no mouse out is recorded" <|
          \() ->
            Expect.equal initialModel.mouseOuts 0
        , test "the event updates the model" <|
          \() ->
            Markup.target ".button" initialState
              |> Event.moveMouseOut
              |> Elmer.expectModel (\model ->
                  Expect.equal model.mouseOuts 1
                )
        , test "it records a position of (0, 0)" <|
          \() ->
            Elmer.given initialModel App.viewForPosition App.update
              |> Markup.target ".button"
              |> Event.moveMouseOut
              |> Elmer.expectModel (\model ->
                Expect.equal initialModel.position Nothing
                  |> andExpect (Expect.equal model.position <| Just { x = 0, y = 0 })
                )
        ]
      ]
  ]
