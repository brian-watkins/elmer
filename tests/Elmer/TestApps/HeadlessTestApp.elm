port module Elmer.TestApps.HeadlessTestApp exposing (..)

import Http
import Json.Decode as Json


port testPortCommand : String -> Cmd msg

spyCommand : String -> Cmd Msg
spyCommand message =
  testPortCommand message


type Msg
  = HttpRequest (Result Http.Error (List Int))

httpCommand : String -> Cmd Msg
httpCommand url =
  Http.get url (Json.list Json.int)
    |> Http.send HttpRequest
