port module Elmer.TestApps.WorkerTestApp exposing (..)

type Msg
  = ReceivedData String

type alias Model =
  { data: String
  }

port incomingData : (String -> msg) -> Sub msg

initialModel : Model
initialModel =
  { data = ""
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedData data ->
      ( { model | data = data }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  incomingData ReceivedData
