module Elmer.TestApps.InputTestApp exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events exposing (onInput, on, keyCode)
import Json.Decode as Json

type alias Model =
  { name : String
  , lastLetter : Int
  }

type Msg
  = DoInput String
  | DoKeyUp Int

defaultModel : Model
defaultModel =
  { name = ""
  , lastLetter = -1
  }

view : Model -> Html Msg
view model =
  Html.div [Attr.id "root"]
    [ Html.input [ Attr.class "nameField", onInput DoInput, onKeyUp DoKeyUp ] []
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoInput input ->
      ( { model | name = input }, Cmd.none )
    DoKeyUp char ->
      ( { model | lastLetter = char }, Cmd.none )

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.map tagger keyCode)
