module Elmer.TestApps.TimeTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Time exposing (Posix)
import Task exposing (Task)

type alias Model =
  { time : Int }

type Msg
  = GetTime
  | NewTime Posix

defaultModel : Model
defaultModel =
  { time = 0 }

view : Model -> Html Msg
view model =
  Html.div [Attr.id "root"]
    [ Html.div [ Attr.id "currentTime" ] [ Html.text ("Time: " ++ (String.fromInt model.time)) ]
    , Html.div [ Attr.class "button", onClick GetTime ] [ Html.text "Click me for the time!" ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GetTime ->
      ( model, Task.perform NewTime Time.now )
    NewTime time ->
      ( { model | time = Time.posixToMillis time }, Cmd.none )
