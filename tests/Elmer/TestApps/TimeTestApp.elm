module Elmer.TestApps.TimeTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Time exposing (Time)
import Task exposing (Task)

type alias Model =
  { time : Time }

type Msg
  = GetTime
  | NewTime Time

defaultModel : Model
defaultModel =
  { time = Time.second }

view : Model -> Html Msg
view model =
  Html.div [Attr.id "root"]
    [ Html.div [ Attr.id "currentTime" ] [ Html.text ("Time: " ++ (toString model.time)) ]
    , Html.div [ Attr.class "button", onClick GetTime ] [ Html.text "Click me for the time!" ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GetTime ->
      ( model, Task.perform NewTime Time.now )
    NewTime time ->
      ( { model | time = time }, Cmd.none )
