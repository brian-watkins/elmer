port module Elmer.TestApps.HeadlessTestApp exposing (..)

import Http

port testPortCommand : String -> Cmd msg

spyCommand : String -> Cmd Msg
spyCommand message =
  testPortCommand message


type Msg
  = HttpRequest (Result Http.Error String)

httpCommand : String -> Cmd Msg
httpCommand url =
  Http.getString url
    |> Http.send HttpRequest
