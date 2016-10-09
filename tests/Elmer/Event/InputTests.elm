module Elmer.Event.InputTests exposing (all)

import Test exposing (..)
import Elmer.TestApp as App
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer.Types exposing (..)
import Elmer.Event.Input as InputEvent
import Elmer

all : Test
all =
  describe "Input Event Tests"
    [ inputEventTests ]

inputEventTests =
  let
    html = App.view App.defaultModel
  in
    describe "input event"
    [ describe "when there is no element"
      [ test "it returns nothing" <|
        \() ->
          Elmer.findResult html ".nothing"
            |> InputEvent.inputResult "fun stuff"
            |> Expect.equal (EventFailure "No html node found with selector: .nothing")
      ]
    , describe "when there is no input event"
      [ test "it returns nothing" <|
        \() ->
          Elmer.findResult html ".awesome"
            |> InputEvent.inputResult "fun stuff"
            |> Expect.equal (EventFailure "No input event found")
      ]
    , describe "when there is an input event"
      [ test "it returns the text tagged with the message" <|
        \() ->
          Elmer.findResult html ".nameField"
            |> InputEvent.inputResult "fun stuff"
            |> Expect.equal (Message (App.HandleInput "fun stuff"))
      ]
    ]
