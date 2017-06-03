module Elmer.HtmlTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.ComponentState as ComponentState exposing (ComponentState)
import Elmer.Html as Markup
import Elmer exposing ((<&&>))
import Elmer.Html.Matchers as Matchers exposing (..)
import Elmer.Html.Query as Query exposing (HtmlTarget(..))
import Elmer.TestHelpers exposing (..)
import Elmer.TestApps.SimpleTestApp as SimpleApp
import Elmer.TestApps.SpyTestApp as SpyApp
import Elmer.Spy as Spy
import Elmer.Spy.Matchers exposing (wasCalled, wasCalledWith, typedArg)
import Html.Attributes as Attr
import Html exposing (Html)


selectTests =
  describe "select"
  [ describe "when there is an upstream failure"
    [ test "it returns the failure" <|
      \() ->
        let
          initialState = ComponentState.failure "upstream failure"
        in
          Markup.target ".button" initialState
            |> Expect.equal initialState
    ]
  ]

expectTests =
  describe "expect"
  [ describe "when there is an upstream failure"
    [ test "it fails with the error message" <|
      \() ->
        let
          initialState = ComponentState.failure "upstream failure"
        in
          initialState
            |> Markup.expect (\context -> Expect.fail "Should not get here")
            |> Expect.equal (Expect.fail "upstream failure")
    , describe "when the matcher uses expectNot"
      [ test "it fails with the right message" <|
        \() ->
          let
            initialState = ComponentState.failure "upstream failure"
          in
            initialState
              |> Markup.target "#no-element"
              |> Markup.expect (Elmer.expectNot elementExists)
              |> Expect.equal (Expect.fail "upstream failure")
      ]
    ]
  , describe "when there is no targeted element"
    [ test "it fails" <|
      \() ->
        Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          |> Markup.expect (\context -> Expect.fail "Should not get here")
          |> Expect.equal (Expect.fail "No expectations could be made because no Html has been targeted.\n\nUse Elmer.Html.target to identify the Html you want to describe.")
    ]
  , describe "when there is a targeted element"
    [ test "it defines the HtmlContext based on the selector and the rendered view" <|
      \() ->
        Elmer.componentState SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          |> Markup.target "#root"
          |> Markup.expect (\query ->
              Expect.equal (Query.forHtml "#root" (SimpleApp.view SimpleApp.defaultModel)) query
            )
          |> Expect.equal Expect.pass
    ]
  ]

childNodeTests =
  describe "nodes with children"
  [ describe "when there is a child node with text"
    [ test "it finds the text" <|
      \() ->
        let
          initialState = Elmer.componentState SimpleApp.defaultModel SimpleApp.viewWithChildren SimpleApp.update
        in
          Markup.target "#root" initialState
            |> Markup.expect (element <| hasText "Child text")
            |> Expect.equal Expect.pass
    ]
  ]

renderTests =
  describe "render"
  [ describe "when there is an upstream failure"
    [ test "it passes on the failure" <|
      \() ->
        let
          initialState = ComponentState.failure "You failed!"
        in
          initialState
            |> Markup.render
            |> Expect.equal initialState
    ]
  , describe "when there is no upstream failure"
    [ test "it renders the view" <|
      \() ->
        let
          spy = Spy.create "view-spy" (\_ -> SimpleApp.view)
        in
          Elmer.componentState SimpleApp.defaultModel (\model -> SimpleApp.view model) SimpleApp.update
            |> Spy.use [ spy ]
            |> Markup.render
            |> Spy.expect "view-spy" (wasCalledWith [ typedArg SimpleApp.defaultModel ])
    ]
  ]
