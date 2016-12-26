module Elmer.TestApps.EmptyTestApp exposing (..)

import Html exposing (Html)

type alias Model =
  { name : String }

type Msg = Msg

defaultModel : Model
defaultModel =
  { name = "Cool Person" }

view : Model -> Html Msg
view model =
  Html.div [] []

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )
