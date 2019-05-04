module Elmer.TestApps.InitTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Task exposing (Task)


type alias Model =
  { name : String
  , baseUrl : String
  , token : String
  }

type Msg
  = TokenRequest (Result String String)
  | Tag String

defaultModel : String -> Model
defaultModel baseUrl =
  { name = "Cool Person"
  , baseUrl = baseUrl
  , token = ""
  }

type alias Flags =
  { baseUrl: String
  }

init : Flags -> ( Model, Cmd Msg )
init flags =
  ( defaultModel flags.baseUrl, requestTokenTask "/fun/token" |> Task.attempt TokenRequest )

requestTokenTask : String -> Task String String
requestTokenTask path =
  Task.succeed "Succeed"

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "base-url" ]
    [ Html.text model.baseUrl ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    TokenRequest result ->
      case result of
        Ok token ->
          ( { model | token = token }, Cmd.none )
        _ ->
          ( model, Cmd.none )
    _ ->
      ( model, Cmd.none )
