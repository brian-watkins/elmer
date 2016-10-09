module Elmer.Event.ClickTests exposing (all)

import Test exposing (..)
import Elmer.TestApp as App
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer.Types exposing (..)
import Elmer.Event.Click as ClickEvent exposing (click)
import Elmer

all : Test
all =
  describe "Click Event Tests"
    [ clickTests
    , clickEventTests
    ]

clickEventTests =
  describe "click event"
  [ describe "when there is nothing found"
    [ test "it returns nothing" <|
      \() ->
        let
          html = App.view App.defaultModel
        in
          Elmer.findResult html "#nothing"
            |> ClickEvent.clickResult
            |> Expect.equal (EventFailure "No html node found with selector: #nothing")
    ]
  , describe "when there is no click event"
    [ test "it returns nothing" <|
      \() ->
        let
          html = App.view App.defaultModel
        in
          Elmer.findResult html ".awesome"
            |> ClickEvent.clickResult
            |> Expect.equal (EventFailure "No click event found")
    ]
  , describe "when there is a click event"
    [ test "it returns the message" <|
      \() ->
        let
          html = App.view App.defaultModel
        in
          Elmer.findResult html ".button"
            |> ClickEvent.clickResult
            |> Expect.equal (Message App.HandleClick)
    ]
  ]


clickTests =
  describe "click based on component state"
  [ describe "when there is an upstream failure"
    [ test "it passes on the error" <|
      \() ->
        let
          initialState = UpstreamFailure "upstream failure"
        in
          ClickEvent.click initialState
            |> Expect.equal initialState
    ]
  , describe "when there is no target node"
    [ test "it returns an upstream failure" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          ClickEvent.click initialState
           |> Expect.equal (UpstreamFailure "No target node specified")
    ]
  , describe "when there is a target node"
    [ describe "when the click fails"
      [ test "it returns an upstream failure" <|
        \() ->
          let
            initialState = Elmer.componentState App.defaultModel App.view App.update
          in
            Elmer.find ".awesome" initialState
              |> ClickEvent.click
              |> Expect.equal (UpstreamFailure "No click event found")
      ]
    ]
    , describe "when the click succeeds"
      [ test "it updates the model accordingly" <|
        \() ->
          let
            initialState = Elmer.componentState App.defaultModel App.view App.update
            updatedStateResult = Elmer.find ".button" initialState
                                  |> ClickEvent.click
          in
            case updatedStateResult of
              CurrentState updatedState ->
                Expect.equal updatedState.model.clicks 1
              UpstreamFailure msg ->
                Expect.fail msg
      ]
  ]
