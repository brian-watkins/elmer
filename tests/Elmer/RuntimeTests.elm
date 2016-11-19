module Elmer.RuntimeTests exposing (all)

import Test exposing (..)
import Expect
import Task

import Elmer exposing (..)
import Elmer.TestApp as App
import Elmer.Event as Event
import Elmer.Matchers as Matchers
import Elmer.Navigation as ElmerNav
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Navigation

all =
  describe "Runtime tests"
  [ mapCommand
  ]

type MsgA
  = DoFun MsgB
  | DoStuff String
  | DoClick
  | DoChangeLocation String

type MsgB
  = HaveFun String
  | HaveError String

type MsgC
  = MsgAWrapper MsgA
  | Error String
  | ShowFunRoute
  | RouteNotFound String

type alias Model =
  { fun: String
  }

defaultModel : Model
defaultModel =
  { fun = "Reading"
  }

subTask : Cmd MsgB
subTask =
  Task.perform HaveFun (Task.succeed "bowling")

update : MsgA -> Model -> (Model, Cmd MsgA)
update message model =
  case message of
    DoFun bMessage ->
      case bMessage of
        HaveFun text ->
          ( { model | fun = text }, Cmd.none )
        HaveError error ->
          ( model, Cmd.none )
    DoStuff stuff ->
      ( model, Cmd.none )
    DoClick ->
      ( { model | fun = "click" }, Cmd.none )
    DoChangeLocation location ->
      ( model, Navigation.newUrl location )

view : Model -> Html MsgA
view model =
  Html.div [ Attr.id "root" ]
    [ Html.p []
      [ Html.text "Here's something fun ..."
      ]
    , Html.div [ Attr.id "click-display", onClick DoClick ]
      [ Html.text ("Fun: " ++ model.fun)
      ]
    , Html.div [ Attr.id "change-location", onClick (DoChangeLocation "http://fun.com/fun.html") ]
      [ Html.text "Click for fun"
      ]
    ]

type Route
  = NotFound String
  | DefaultRoute
  | FunRoute

type alias ParentModel =
  { childModel: Model
  , route: Route
  }

defaultParentModel : ParentModel
defaultParentModel =
  { childModel = defaultModel
  , route = DefaultRoute
  }

parentUpdate : MsgC -> ParentModel -> (ParentModel, Cmd MsgC)
parentUpdate message model =
  case message of
    MsgAWrapper msgA ->
      let
        (updatedChildModel, childCommand) = update msgA model.childModel
      in
        ( { model | childModel = updatedChildModel }, Cmd.map MsgAWrapper childCommand )
    Error _ ->
      ( model, Cmd.none )
    ShowFunRoute ->
      ( { model | route = FunRoute }, Cmd.none )
    RouteNotFound message ->
      ( { model | route = NotFound message }, Cmd.none )


parentView : ParentModel -> Html MsgC
parentView model =
  case model.route of
    DefaultRoute ->
      Html.div [ Attr.id "parent-root"]
        [ Html.p [] [ Html.text "Parent view"]
        , Html.div [ Attr.id "child-view" ]
          [ Html.map MsgAWrapper (view model.childModel) ]
        ]
    FunRoute ->
      Html.div [ Attr.id "fun-stuff" ]
        [ Html.p [] [ Html.text "Fun things!" ] ]
    NotFound message ->
      Html.div [ Attr.id "not-found-error" ]
        [ Html.p [] [ Html.text ("Page not found" ++ message) ] ]

parseLocation : Navigation.Location -> MsgC
parseLocation location =
  if location.href == "http://fun.com/fun.html" then
    ShowFunRoute
  else
    RouteNotFound ("Unknown url: " ++ location.href)



mapCommand =
  describe "Map Command"
  [ describe "within a single component"
    [ test "it handles a map command" <|
      \() ->
        let
          initialState = Elmer.componentState defaultModel view update
          mapCommand = Cmd.map DoFun subTask
        in
          Event.sendCommand mapCommand initialState
            |> Elmer.find "#root"
            |> Elmer.expectNode (Matchers.hasText "Fun: bowling")
    , test "it handles a click event" <|
      \() ->
        let
          initialState = Elmer.componentState defaultModel view update
          mapCommand = Cmd.map DoFun subTask
        in
          Event.sendCommand mapCommand initialState
            |> Elmer.find "#click-display"
            |> Event.click
            |> Elmer.find "#root"
            |> Elmer.expectNode (Matchers.hasText "Fun: click")
    ]
  , describe "when a child component is used by the parent"
    [ test "it handles a mapped map command" <|
      \() ->
        let
          initialState = Elmer.componentState defaultParentModel parentView parentUpdate
          mapCommand = Cmd.map DoFun subTask
          parentMapCommand = Cmd.map MsgAWrapper mapCommand
        in
          Event.sendCommand parentMapCommand initialState
            |> Elmer.find "#child-view"
            |> Elmer.expectNode (Matchers.hasText "Fun: bowling")
    , test "it handles a mapped message from the child view" <|
      \() ->
        let
          initialState = Elmer.componentState defaultParentModel parentView parentUpdate
          mapCommand = Cmd.map DoFun subTask
          parentMapCommand = Cmd.map MsgAWrapper mapCommand
        in
          Event.sendCommand parentMapCommand initialState
            |> Elmer.find "#click-display"
            |> Event.click
            |> Elmer.find "#child-view"
            |> Elmer.expectNode (Matchers.hasText "Fun: click")
    , describe "when the mapped command has a custom update method"
      [ test "it handles a mapped message from the child view" <|
        \() ->
          let
            initialState = navigationComponentState defaultParentModel parentView parentUpdate parseLocation
          in
            Elmer.find "#change-location" initialState
              |> Event.click
              |> Elmer.find "#fun-stuff"
              |> Elmer.expectNode (Matchers.hasText "Fun things!")
      ]
    ]
  ]
