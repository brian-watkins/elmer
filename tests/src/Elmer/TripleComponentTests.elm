module Elmer.TripleComponentTests exposing (..)

import Test exposing (..)
import Expect

import Elmer exposing (..)
import Elmer.Html.Event as Event
import Elmer.Html.Matchers as Matchers exposing (element, hasText)
import Elmer.Html.Selector as Sel exposing (..)
import Elmer.Spy as Spy
import Elmer.Command as Command
import Elmer.Html as Markup
import Elmer.Navigation as ElmerNav

import Elmer.TestApps.TripleComponentTestApp as App exposing (..)

all : Test
all =
  Test.concat
  [ appTests
  ]


appTests : Test
appTests =
  describe "Triple component"
  [ test "it handles a click from the grandchild component" <|
    \() ->
      Elmer.given App.defaultModel App.view App.update
        |> Markup.target << by [ Sel.tag "button" ]
        |> Event.click
        |> Markup.target << by [ Sel.id "grand-child-name" ]
        |> Markup.expect (element <| hasText "Handled Click")
  ]
