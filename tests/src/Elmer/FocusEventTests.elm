module Elmer.FocusEventTests exposing (..)

import Test exposing (..)
import Elmer.TestApps.FocusTestApp as App
import Expect
import Elmer
import Elmer.EventTests as EventTests
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Html.Event as Event
import Elmer.Command as Command
import Elmer.Html as Markup
import Elmer.Html.Selector exposing (..)


all : Test
all =
  Test.concat
  [ focusTests
  , blurTests
  ]

focusTests : Test
focusTests =
  describe "focus"
  [ EventTests.standardEventBehavior "focus" Event.focus
  , EventTests.propagationBehavior Event.focus "focus"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.view App.update
    in
      describe "the focus event"
      [ test "at first the element is not focused" <|
        \() ->
          Expect.equal initialModel.isFocused False
      , test "the event updates the model" <|
        \() ->
          initialState
            |> Markup.target << by [ id "name-field" ]
            |> Event.focus
            |> Elmer.expectModel (\model ->
                Expect.equal model.isFocused True
              )
      ]
  ]

blurTests : Test
blurTests =
  describe "blur"
  [ EventTests.standardEventBehavior "blur" Event.blur
  , EventTests.propagationBehavior Event.blur "blur"
  , let
      initialModel = App.defaultModel
      initialState = Elmer.given initialModel App.view App.update
    in
      describe "the blur event"
      [ test "at first the element is not blurred" <|
        \() ->
          Expect.equal initialModel.isBlurred False
      , test "the event updates the model" <|
        \() ->
          initialState
            |> Markup.target << by [ id "name-field" ]
            |> Event.blur
            |> Elmer.expectModel (\model ->
                Expect.equal model.isBlurred True
              )
      ]
  ]
