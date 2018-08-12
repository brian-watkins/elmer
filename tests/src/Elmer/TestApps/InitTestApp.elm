module Elmer.TestApps.InitTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Http

type alias Model =
  { name : String
  , baseUrl : String
  }

type Msg
  = TokenRequest (Result Http.Error String)
  | Tag String

defaultModel : String -> Model
defaultModel baseUrl =
  { name = "Cool Person"
  , baseUrl = baseUrl
  }

type alias Flags =
  { baseUrl: String
  }

init : Flags -> ( Model, Cmd Msg )
init flags =
  ( defaultModel flags.baseUrl, Http.send TokenRequest <| Http.getString (flags.baseUrl ++ "/token") )

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "base-url" ]
    [ Html.text model.baseUrl ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )
