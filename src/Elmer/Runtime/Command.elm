module Elmer.Runtime.Command exposing
  ( commandRunner
  , mapState
  , generate
  , fail
  , stub
  )

import Elmer.Runtime.Intention as Intention
import Elmer.Runtime.Types exposing (..)
import Elmer.Runtime.Command.Task
import Elmer.Runtime.Command.MapState
import Elmer.Runtime.Command.Generate
import Elmer.Runtime.Command.Fail
import Elmer.Runtime.Command.Stub
import Elmer.Context as Context exposing (Context)
import Elmer.Printer exposing (..)
import Dict exposing (Dict)


commandRunner : String -> CommandRunner model subMsg msg
commandRunner name =
  case name of
    "Elmer_Fail" ->
      Elmer.Runtime.Command.Fail.commandRunner
    "Elmer_Stub" ->
      Elmer.Runtime.Command.Stub.commandRunner
    "Elmer_Generate" ->
      Elmer.Runtime.Command.Generate.commandRunner
    "Elmer_MapState" ->
      Elmer.Runtime.Command.MapState.commandRunner
    "Task" ->
      Elmer.Runtime.Command.Task.commandRunner
    unknownCommand ->
      unknownCommandRunner unknownCommand


mapState : typeId -> (Maybe a -> a) -> Cmd msg
mapState typeId mapper =
  Intention.toCmd "Elmer_MapState" { typeId = typeId, mapper = mapper }


generate : (Context model msg -> Cmd msg) -> Cmd msg
generate generator =
  Intention.toCmd "Elmer_Generate" generator


fail : String -> Cmd msg
fail =
  Intention.toCmd "Elmer_Fail"


stub : msg -> Cmd msg
stub =
  Intention.toCmd "Elmer_Stub"


unknownCommandRunner : String -> CommandRunner model subMsg msg
unknownCommandRunner commandName _ _ =
  CommandError <|
    format <|
      [ message "Elmer encountered a command it does not know how to run" commandName
      , description "Try sending a stubbed or dummy command instead"
      ]
