module Elmer.ComponentTests exposing (all)

import Test exposing (..)
import Expect

import Elmer exposing (..)
import Elmer.Html.Event as Event
import Elmer.Command as Command
import Elmer.Html.Matchers as Matchers
import Elmer.Command as Command
import Elmer.Html as Markup

import Elmer.TestApps.ComponentTestApp as App exposing (..)

all =
  describe "Component tests"
  [ mapCommand
  ]


subTask : Cmd App.MsgB
subTask =
  Command.stub (HaveFun "bowling")


mapCommand =
  describe "Map Command"
  [ describe "within a single component"
    [ test "it handles a map command" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
          mapCommand = Cmd.map DoFun subTask
        in
          Command.send mapCommand initialState
            |> Markup.find "#root"
            |> Markup.expectNode (Matchers.hasText "Fun: bowling")
    , test "it handles a click event" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
          mapCommand = Cmd.map DoFun subTask
        in
          Command.send mapCommand initialState
            |> Markup.find "#click-display"
            |> Event.click
            |> Markup.find "#root"
            |> Markup.expectNode (Matchers.hasText "Fun: click")
    ]
  , describe "when a child component is used by the parent"
    [ test "it handles a mapped map command" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultParentModel App.parentView App.parentUpdate
          mapCommand = Cmd.map DoFun subTask
          parentMapCommand = Cmd.map MsgAWrapper mapCommand
        in
          Command.send parentMapCommand initialState
            |> Markup.find "#child-view"
            |> Markup.expectNode (Matchers.hasText "Fun: bowling")
    , test "it handles a mapped message from the child view" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultParentModel App.parentView App.parentUpdate
          mapCommand = Cmd.map DoFun subTask
          parentMapCommand = Cmd.map MsgAWrapper mapCommand
        in
          Command.send parentMapCommand initialState
            |> Markup.find "#click-display"
            |> Event.click
            |> Markup.find "#child-view"
            |> Markup.expectNode (Matchers.hasText "Fun: click")
    , describe "when the mapped command has a custom update method"
      [ test "it handles a mapped message from the child view" <|
        \() ->
          let
            initialState = navigationComponentState App.defaultParentModel App.parentView App.parentUpdate App.parseLocation
          in
            Markup.find "#change-location" initialState
              |> Event.click
              |> Markup.find "#fun-stuff"
              |> Markup.expectNode (Matchers.hasText "Fun things!")
      ]
    ]
  ]
