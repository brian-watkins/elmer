module Elmer.TestApps.ComponentTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (Document, UrlRequest(..))
import Url exposing (Url)

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
  | OnUrlRequest UrlRequest
  | OnUrlChange Url

type alias Model =
  { fun: String
  }

defaultModel : Model
defaultModel =
  { fun = "Reading"
  }

update : Navigation.Key -> MsgA -> Model -> (Model, Cmd MsgA)
update navigationKey message model =
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
      ( model, Navigation.pushUrl navigationKey location )

simpleUpdate : MsgA -> Model -> (Model, Cmd MsgA)
simpleUpdate msg model =
  case msg of
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
    DoChangeLocation _ ->
      ( model, Cmd.none )


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
  , navigationKey: Navigation.Key
  }

parentUpdate : MsgC -> ParentModel -> (ParentModel, Cmd MsgC)
parentUpdate message model =
  case message of
    MsgAWrapper msgA ->
      let
        (updatedChildModel, childCommand) = update model.navigationKey msgA model.childModel
      in
        ( { model | childModel = updatedChildModel }, Cmd.map MsgAWrapper childCommand )
    Error _ ->
      ( model, Cmd.none )
    OnUrlRequest urlRequest ->
      case urlRequest of
        Internal url ->
          ( model, Navigation.pushUrl model.navigationKey (Url.toString url) )
        External _ ->
          ( model, Cmd.none )
    OnUrlChange url ->
      if url.host == "fun.com" && url.path == "/fun.html" then
        ( { model | route = FunRoute }, Cmd.none )
      else
        ( { model | route = NotFound <| "Unknown url: " ++ Url.toString url }, Cmd.none )


parentDocument : ParentModel -> Document MsgC
parentDocument model =
  { title = "Component Test App"
  , body = [ parentView model ]
  }


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


init : () -> Url -> Navigation.Key -> (ParentModel, Cmd MsgC)
init _ url key =
  ( { childModel = defaultModel
    , route = DefaultRoute
    , navigationKey = key
    }
  , Cmd.none
  )
