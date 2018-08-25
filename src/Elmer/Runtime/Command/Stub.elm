module Elmer.Runtime.Command.Stub exposing
  ( with
  , commandRunner
  , name
  )

import Elmer.Runtime.Types exposing (..)
import Elmer.Runtime.Intention as Intention
import Elmer.Context as Context exposing (Context)
import Elmer.Runtime.Command.Fail as Fail
import Elmer.Errors as Errors


name : String
name =
  "Elmer_Stub"


with : msg -> Cmd msg
with =
  Intention.toCmd name


commandRunner : CommandRunner model subMsg msg
commandRunner command tagger =
  let
    msg = 
      Intention.cmdValue command
        |> tagger
  in
    processStub msg
      |> CommandSuccess


processStub : msg -> CommandEffect model msg
processStub msg context =
  case Context.update msg context of
    Just tuple ->
      tuple
    Nothing ->
      (context, Fail.with Errors.noModel)