module Elmer.TestApps.FocusTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onFocus)

type alias Model =
  { isFocused : Bool }

type Msg
  = Focused

defaultModel : Model
defaultModel =
  { isFocused = False }

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
    [ Html.input [ Attr.id "name-field", Attr.type_ "text", onFocus Focused ] [] ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Focused ->
      ( { model | isFocused = True }, Cmd.none )
