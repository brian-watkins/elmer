module Elmer.AppTests exposing (all)

import Test exposing (..)
import Elmer.TestApp as App
import Elmer.TestHelpers exposing (..)
import Expect
import Elmer exposing (..)
import Elmer.Event as Event
import Elmer.Matchers as Matchers
import Elmer.TestExtra as ElmerTest

all : Test
all =
  describe "App Flow tests"
    [ appFlowTests ]

appFlowTests =
  describe "app flow"
    [ test "it updates the model as events are processed and passes the expectation" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Elmer.find ".button" initialState
            |> Event.click
            |> Event.click
            |> Elmer.find "#clickCount"
            |> Elmer.expectNode (Matchers.hasText "2 clicks!")
            |> Expect.equal (Expect.pass)
    , test "it makes multiple expectations about a node" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Elmer.find ".withText" initialState
            |> Elmer.expectNode (
              \node ->
                Matchers.hasText "Some Fun Text" node
                  |> ElmerTest.andThen (Matchers.hasText "special!" node)
                  |> ElmerTest.andThen (Matchers.hasText "link to fun!" node)
            )
    , let
        initialState = Elmer.componentState App.defaultModel App.view App.update
        resultState = Elmer.find "#numberButton" initialState
          |> Event.click
      in
        describe "command with successful task"
        [ test "it displays the number" <|
            \() ->
              Elmer.find "#numberOutput" resultState
                |> Elmer.expectNode (
                  \node ->
                    Matchers.hasText "Clicked and got number: 3" node
                )
        , test "it does not display an error" <|
            \() ->
              Elmer.find "#numberOutputError" resultState
                |> Elmer.expectNode (
                  \node ->
                    Matchers.hasText "Got error requesting number: No error" node
                )
        ]
    , let
        defaultModel = App.defaultModel
        modelWithFailingTask = { defaultModel | numberTaskGenerator = (App.makeNumberTaskThatSucceeds False) }
        initialState = Elmer.componentState modelWithFailingTask App.view App.update
        resultState = Elmer.find "#numberButton" initialState
          |> Event.click
      in
        describe "command with unsuccessful task"
        [ test "it does not display a number" <|
            \() ->
              Elmer.find "#numberOutput" resultState
                |> Elmer.expectNode (
                  \node ->
                    Matchers.hasText "Clicked and got number: -1" node
                )
        , test "it does display an error" <|
            \() ->
              Elmer.find "#numberOutputError" resultState
                |> Elmer.expectNode (
                  \node ->
                    Matchers.hasText "Got error requesting number: Bad things happened!" node
                )
        ]
    ]
