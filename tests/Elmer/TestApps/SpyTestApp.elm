module Elmer.TestApps.SpyTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

type alias Model =
  { name : Maybe String }

type Msg
  = HandleClick

defaultModel : Model
defaultModel =
  { name = Just "Cool Person" }

view : Model -> Html Msg
view model =
  Html.div
    [ Attr.id "root", Attr.class "styled no-events" ]
    [ Html.div [ Attr.id "title" ] [ Html.text <| titleText "Some Title" ]
    , Html.div [ Attr.id "button", Events.onClick HandleClick ] [ Html.text "Click me to clear!" ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HandleClick ->
      ( clearName "Default Name" model, Cmd.none )

titleText : String -> String
titleText name =
  "A Title: " ++ name

clearName : String -> Model -> Model
clearName default model =
  { model | name = Just default }
