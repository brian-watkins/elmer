module Elmer.TestApps.NavigationTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Navigation

type alias Model =
  { error: String }

type Msg
  = DoClick
  | ViewRoute
  | RouteNotFound String

defaultModel : Model
defaultModel =
  { error = "" }

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
    [ Html.div [ Attr.id "navigateButton", onClick DoClick ] [ Html.text "Click to navigate!" ]
    , Html.div [ Attr.class "error" ] [ Html.text model.error ]
    ]

updateWithDependencies : (String -> Cmd Msg) -> Msg -> Model -> ( Model, Cmd Msg )
updateWithDependencies navigateFunction msg model =
  case msg of
    DoClick ->
      ( model, navigateFunction "http://fun.com/fun.html" )
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
