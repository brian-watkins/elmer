port module Elmer.TestApps.PortTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

port sendJsCommand : String -> Cmd msg

type alias Model =
  { name : String }

type Msg
  = HandleClick

defaultModel : Model
defaultModel =
  { name = "Cool Person" }

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
    [ Html.div [ Attr.id "send-port-command-button", Events.onClick HandleClick ]
      [ Html.text "Click me!" ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HandleClick ->
      ( model, sendJsCommand "Hey!" )
