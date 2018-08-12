module Elmer.TestApps.PortTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

-- port sendJsData : String -> Cmd msg
sendJsData : String -> Cmd msg
sendJsData _ =
  Cmd.none

-- port receiveJsData : (String -> msg) -> Sub msg
receiveJsData : (String -> msg) -> Sub msg
receiveJsData _ =
  Sub.none


type alias Model =
  { name : String
  , jsData : String
  }

type Msg
  = HandleClick
  | ReceivedData String

defaultModel : Model
defaultModel =
  { name = "Cool Person"
  , jsData = ""
  }

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
    [ Html.div [ Attr.id "send-port-command-button", Events.onClick HandleClick ]
      [ Html.text "Click me!" ]
    , Html.div [ Attr.id "js-data" ]
      [ Html.text model.jsData ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HandleClick ->
      ( model, sendJsData "Hey!" )
    ReceivedData message ->
      ( { model | jsData = message }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  receiveJsData ReceivedData
