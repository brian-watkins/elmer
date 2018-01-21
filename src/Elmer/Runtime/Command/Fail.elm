module Elmer.Runtime.Command.Fail exposing
  ( with
  , commandRunner
  , name
  )

import Elmer.Runtime.Types exposing (..)
import Elmer.Runtime.Intention as Intention
import Elmer.Context as Context exposing (Context)


name : String
name =
  "Elmer_Fail"

commandRunner : CommandRunner model subMsg msg
commandRunner command _ =
  let
    message = Intention.cmdValue command
  in
    CommandError message

with : String -> Cmd msg
with =
  Intention.toCmd name
