module Elmer.TestApps.NavigationTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Navigation

type alias Model =
  { error: String }

type Msg
  = DoNewUrl
  | DoModifyUrl
  | ViewRoute
  | RouteNotFound String

defaultModel : Model
defaultModel =
  { error = "" }

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
    [ Html.div [ Attr.id "navigateButton", onClick DoNewUrl ] [ Html.text "Click to navigate!" ]
    , Html.div [ Attr.id "modifyLocationButton", onClick DoModifyUrl ] [ Html.text "Click to also navigate!" ]
    , Html.div [ Attr.class "error" ] [ Html.text model.error ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoNewUrl ->
      ( model, Navigation.newUrl "http://fun.com/fun.html" )
    DoModifyUrl ->
      ( model, Navigation.modifyUrl "http://fun.com/awesome.html" )
    ViewRoute ->
      ( { model | error = "No error" }, Cmd.none )
    RouteNotFound error ->
      ( { model | error = error }, Cmd.none )

parseLocation : Navigation.Location -> Msg
parseLocation location =
  if location.pathname == "/api/view" then
    ViewRoute
  else
    RouteNotFound ("Unknown path: " ++ location.pathname)
