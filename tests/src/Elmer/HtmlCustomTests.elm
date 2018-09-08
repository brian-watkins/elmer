module Elmer.HtmlCustomTests exposing (..)

import Test exposing (..)
import Expect
import Elmer
import Elmer.Html as Markup
import Elmer.Html.Event as Event
import Elmer.Html.Element as Element
import Elmer.Html.Selector as Sel exposing (by)
import Elmer.Html.Matchers exposing (element, hasText, hasAttribute, hasStyle, listensForEvent)
import Elmer.TestApps.CustomElementTestApp as App


all : Test
all =
  Test.concat
  [ customElementTests
  ]

customElementTests : Test
customElementTests =
  describe "custom element"
  [ test "it creates a div for the custom content with the expected property" <|
    \() ->
      Elmer.given testModel App.view App.update
        |> Markup.target << by [ Sel.id "markdown-content" ]
        |> Markup.expect (element <| \el -> Expect.equal "div" <| Element.tag el)
  , test "it creates a div for the custom content with the expected attributes" <|
    \() ->
      Elmer.given testModel App.view App.update
        |> Markup.target << by [ Sel.id "markdown-content" ]
        |> Markup.expect (element <| hasAttribute ("data-attr", "funStuff"))
  , test "it creates a div for the custom content with the expected styles" <|
    \() ->
      Elmer.given testModel App.view App.update
        |> Markup.target << by [ Sel.id "markdown-content" ]
        |> Markup.expect (element <| hasStyle ("position", "absolute"))
  , test "it creates a div for the custom content with the expected event handlers" <|
    \() ->
      Elmer.given testModel App.view App.update
        |> Markup.target << by [ Sel.id "markdown-content" ]
        |> Markup.expect (element <| listensForEvent "click")
  , test "it creates a div that bubbles events as necessary" <|
    \() ->
      Elmer.given testModel App.bubbleView App.update
        |> Markup.target << by [ Sel.id "markdown-content" ]
        |> Event.click
        |> Markup.expect (element <| hasAttribute ("data-attr", "Bubbled"))
  ]

testModel : App.Model
testModel =
  App.defaultModel <| """

# Some Cool Title

Then some cool text.

"""