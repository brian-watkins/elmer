module Elmer.TestApps.TaskTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Task

type alias Model =
  { firstTask : String
  , secondTask : String
  }

defaultModel : Model
defaultModel =
  { firstTask = ""
  , secondTask = ""
  }

type Msg
  = FirstTaskResult String
  | SecondTaskResult String

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
  [ Html.div [ Attr.id "first-task-result" ]
    [ Html.text model.firstTask ]
  , Html.div [ Attr.id "second-task-result" ]
    [ Html.text model.secondTask ]
  ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FirstTaskResult str ->
      ( { model | firstTask = str }, Cmd.none )
    SecondTaskResult str ->
      ( { model | secondTask = str }, Cmd.none )


sendFirstTask : String -> Cmd Msg
sendFirstTask str =
  Task.perform FirstTaskResult (Task.succeed str)

sendSecondTask : String -> Cmd Msg
sendSecondTask str =
  Task.perform SecondTaskResult (Task.succeed str)
