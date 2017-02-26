module Elmer.TestApps.InputTestApp exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events exposing (onInput, on, keyCode, onCheck)
import Json.Decode as Json

type alias Model =
  { name : String
  , lastLetter : Int
  , isChecked : Bool
  }

type Msg
  = DoInput String
  | DoKeyUp Int
  | HandleCheck Bool

defaultModel : Model
defaultModel =
  { name = ""
  , lastLetter = -1
  , isChecked = False
  }

view : Model -> Html Msg
view model =
  Html.div [Attr.id "root"]
    [ Html.input [ Attr.type_ "text", Attr.name "first-name", onInput DoInput, onKeyUp DoKeyUp ] []
    , Html.input
      [ Attr.type_ "checkbox", Attr.name "is-cool", Attr.checked model.isChecked, onCheck HandleCheck ]
      [ Html.text "Are you cool?" ]
    , Html.input [ Attr.type_ "checkbox", Attr.name "no-op" ] [ Html.text "I do nothing." ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoInput input ->
      ( { model | name = input }, Cmd.none )
    DoKeyUp char ->
      ( { model | lastLetter = char }, Cmd.none )
    HandleCheck didCheck ->
      ( { model | isChecked = didCheck }, Cmd.none )

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.map tagger keyCode)
