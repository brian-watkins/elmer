module Elmer.HtmlKeyedTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Html as Markup
import Elmer.Html.Event as Event
import Elmer exposing ((<&&>), hasLength, exactly)
import Elmer.Html.Matchers as Matchers exposing (..)
import Html.Attributes as Attr
import Html exposing (Html)
import Elmer.TestApps.HtmlKeyedTestApp as App


keyedTests : Test
keyedTests =
  describe "keyed nodes"
  [ test "it renders the keyed nodes" <|
    \() ->
      Elmer.given App.defaultModel App.view App.update
        |> Markup.target "#fruit-list li"
        |> Markup.expect (elements <|
          hasLength 3
          <&&> (exactly 1 <| hasText "apple")
          <&&> (exactly 1 <| hasText "pear")
          <&&> (exactly 1 <| hasText "orange")
        )
  , test "it handles inherited events" <|
    \() ->
      Elmer.given App.defaultModel App.view App.update
        |> Markup.target "#fruit-list li"
        |> Event.click
        |> Markup.expect (elements <|
          hasLength 3
          <&&> (exactly 1 <| hasText "apple")
          <&&> (exactly 1 <| hasText "pear")
          <&&> (exactly 1 <| hasText "pineapple")
        )
  , test "it maps events for keyed nodes" <|
    \() ->
      Elmer.given defaultWrappedModel wrappedView wrappedUpdate
        |> Markup.target "#special-node"
        |> Event.click
        |> Markup.target "#fruit-list li"
        |> Markup.expect (elements <|
          hasLength 3
          <&&> (exactly 1 <| hasText "apple")
          <&&> (exactly 1 <| hasText "pear")
          <&&> (exactly 1 <| hasText "popcorn")
        )
  , test "it handles lazy keyed nodes" <|
    \() ->
      Elmer.given App.defaultModel App.viewLazyNode App.update
        |> Markup.target "#fruit-list li"
        |> Markup.expect (elements <|
          hasLength 3
          <&&> (exactly 1 <| hasText "apple")
          <&&> (exactly 1 <| hasText "chocolate")
          <&&> (exactly 1 <| hasText "orange")
        )
  , test "it lazily handles keyed nodes" <|
    \() ->
      Elmer.given App.defaultModel App.lazyKeyedView App.update
        |> Markup.target "#fruit-list li"
        |> Markup.expect (elements <|
          hasLength 3
          <&&> (exactly 1 <| hasText "apple")
          <&&> (exactly 1 <| hasText "grapes")
          <&&> (exactly 1 <| hasText "orange")
        )
  ]


-- Test app for Html.Map and Cmd.map

type TestMsg
  = AppMsg App.Msg

type alias TestModel =
  { appModel : App.Model
  }

defaultWrappedModel : TestModel
defaultWrappedModel =
  { appModel = App.defaultModel
  }

wrappedView : TestModel -> Html TestMsg
wrappedView model =
  Html.div [ Attr.id "app-view" ]
  [ Html.map AppMsg <| App.view2 model.appModel ]

wrappedUpdate : TestMsg -> TestModel -> ( TestModel, Cmd TestMsg )
wrappedUpdate msg model =
  case msg of
    AppMsg appMsg ->
      let
        ( updatedModel, updatedCommand ) = App.update appMsg model.appModel
      in
        ( { model | appModel = updatedModel }
        , Cmd.map AppMsg updatedCommand
        )
