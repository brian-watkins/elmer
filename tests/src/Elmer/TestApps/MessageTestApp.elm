module Elmer.TestApps.MessageTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr

type alias Model =
  { firstMessage : String
  , secondMessage : String
  }

defaultModel : Model
defaultModel =
  { firstMessage = ""
  , secondMessage = ""
  }

type Msg
  = RenderFirstMessage String
  | RenderSecondMessage String

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
  [ Html.div [ Attr.id "first-message" ]
    [ Html.text model.firstMessage ]
  , Html.div [ Attr.id "second-message" ]
    [ Html.text model.secondMessage ]
  ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    RenderFirstMessage str ->
      ( { model | firstMessage = str }, Cmd.none )
    RenderSecondMessage str ->
      ( { model | secondMessage = str }, Cmd.none )
