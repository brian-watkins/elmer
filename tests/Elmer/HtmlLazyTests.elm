module Elmer.HtmlLazyTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Html as Markup
import Elmer.Html.Event as Event
import Elmer exposing ((<&&>))
import Elmer.Html.Matchers as Matchers exposing (..)
import Elmer.Html.Query as Query exposing (HtmlTarget(..))
import Elmer.TestHelpers exposing (..)
import Elmer.Spy as Spy
import Elmer.Spy.Matchers exposing (wasCalled, wasCalledWith, typedArg)
import Html.Attributes as Attr
import Html exposing (Html)
import Elmer.TestApps.LazyTestApp as App


lazyTests : Test
lazyTests =
  describe "When a view uses lazy"
  [ test "it renders the lazy html" <|
    \() ->
      Elmer.given App.defaultModel App.view App.update
        |> Markup.target "#lazy-div"
        |> Markup.expect (element <| hasText "Some name: Cool Person")
  , test "it passes inherited events" <|
    \() ->
      Elmer.given App.defaultModel App.view App.update
        |> Markup.target "#lazy-div"
        |> Event.click
        |> Markup.expect (element <| hasText "Some name: Awesome Person")
  , test "it maps events from lazy html" <|
    \() ->
      Elmer.given defaultWrappedModel wrappedView wrappedUpdate
        |> Markup.target "#lazy-div"
        |> Event.click
        |> Markup.expect (element <| hasText "Happy Cool Person likes bowling")
  ]


lazy2Tests : Test
lazy2Tests =
  describe "When a view uses lazy2"
  [ test "it renders the lazy html" <|
    \() ->
      Elmer.given App.defaultModel App.lazyView2 App.update
        |> Markup.target "#lazy-div"
        |> Markup.expect (element <| hasText "Cool Person likes bowling")
  ]


lazy3Tests : Test
lazy3Tests =
  describe "When a view uses lazy3"
  [ test "it renders the lazy html" <|
    \() ->
      Elmer.given App.defaultModel App.lazyView3 App.update
        |> Markup.target "#root"
        |> Markup.expect (element <| hasText "Cool Person likes bowling 5 times a week!")
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
  [ Html.map AppMsg <| App.lazyView2 model.appModel ]

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
