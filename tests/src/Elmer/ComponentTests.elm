module Elmer.ComponentTests exposing (..)

import Test exposing (..)
import Expect

import Elmer exposing (..)
import Elmer.Html.Event as Event
import Elmer.Platform.Command as Command
import Elmer.Html.Matchers as Matchers exposing (element, hasText)
import Elmer.Spy as Spy
import Elmer.Platform.Command as Command
import Elmer.Html as Markup
import Elmer.Navigation as ElmerNav
import Elmer.UrlHelpers as UrlHelpers
import Elmer.Browser
import Elmer.TestApps.ComponentTestApp as App exposing (..)


all : Test
all =
  Test.concat
  [ mapCommandTest
  ]


subTask : Cmd App.MsgB
subTask =
  Command.fake (HaveFun "bowling")


mapCommandTest =
  describe "Map Command"
  [ describe "within a single component"
    [ test "it handles a map command" <|
      \() ->
        let
          initialState = Elmer.given App.defaultModel App.view App.simpleUpdate
          mapCommand = \() -> Cmd.map DoFun subTask
        in
          Command.send mapCommand initialState
            |> Markup.target "#root"
            |> Markup.expect (element <| hasText "Fun: bowling")
    , test "it handles a click event" <|
      \() ->
        let
          initialState = Elmer.given App.defaultModel App.view App.simpleUpdate
          mapCommand = \() -> Cmd.map DoFun subTask
        in
          Command.send mapCommand initialState
            |> Markup.target "#click-display"
            |> Event.click
            |> Markup.target "#root"
            |> Markup.expect (element <| hasText "Fun: click")
    ]
  , describe "when a child component is used by the parent"
    [ test "it handles a mapped map command" <|
      \() ->
        let
          initialState = 
            Elmer.Browser.givenApplication App.OnUrlRequest App.OnUrlChange App.parentDocument App.parentUpdate
              |> Elmer.Browser.init (\_ -> App.init () (UrlHelpers.asUrl "http://localhost/fun.html") ElmerNav.fakeKey)
          mapCommand = Cmd.map DoFun subTask
          parentMapCommand = \() -> Cmd.map MsgAWrapper mapCommand
        in
          Command.send parentMapCommand initialState
            |> Markup.target "#child-view"
            |> Markup.expect (element <| hasText "Fun: bowling")
    , test "it handles a mapped message from the child view" <|
      \() ->
        let
          initialState = 
            Elmer.Browser.givenApplication App.OnUrlRequest App.OnUrlChange App.parentDocument App.parentUpdate
              |> Elmer.Browser.init (\_ -> App.init () (UrlHelpers.asUrl "http://localhost/fun.html") ElmerNav.fakeKey)
          mapCommand = Cmd.map DoFun subTask
          parentMapCommand = \() -> Cmd.map MsgAWrapper mapCommand
        in
          Command.send parentMapCommand initialState
            |> Markup.target "#click-display"
            |> Event.click
            |> Markup.target "#child-view"
            |> Markup.expect (element <| hasText "Fun: click")
    , describe "when the mapped command has a custom update method"
      [ test "it handles a mapped message from the child view" <|
        \() ->
          Elmer.Browser.givenApplication App.OnUrlRequest App.OnUrlChange App.parentDocument App.parentUpdate
            |> Spy.use [ ElmerNav.spy ]
            |> Elmer.Browser.init (\_ -> App.init () (UrlHelpers.asUrl "http://localhost/fun.html") ElmerNav.fakeKey)
            |> Markup.target "#change-location"
            |> Event.click
            |> Markup.target "#fun-stuff"
            |> Markup.expect (element <| hasText "Fun things!")
      ]
    ]
  ]