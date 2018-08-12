module Elmer.TestApps.FocusTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onFocus, onBlur)

type alias Model =
  { isFocused : Bool
  , isBlurred : Bool
  }

type Msg
  = Focused
  | Blurred

defaultModel : Model
defaultModel =
  { isFocused = False
  , isBlurred = False
  }

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
    [ Html.input [ Attr.id "name-field", Attr.type_ "text", onFocus Focused, onBlur Blurred ] [] ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Focused ->
      ( { model | isFocused = True }, Cmd.none )
    Blurred ->
      ( { model | isBlurred = True }, Cmd.none )
