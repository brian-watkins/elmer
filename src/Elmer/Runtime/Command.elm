module Elmer.Runtime.Command exposing
  ( commandRunner
  , mapContext
  , generate
  , fail
  , stub
  )

import Elmer.Runtime.Intention as Intention
import Elmer.Runtime.Types exposing (..)
import Elmer.Context.Internal as Context exposing (Context)
import Elmer.Printer exposing (..)
import Dict exposing (Dict)


commandRunner : String -> CommandRunner model subMsg msg
commandRunner name =
  case name of
    "Elmer_Fail" -> failCommandRunner
    "Elmer_Stub" -> stubCommandRunner
    "Elmer_Generate" -> generateCommandRunner
    "Elmer_MapContext" -> mapContextCommandRunner
    unknownCommand -> unknownCommandRunner unknownCommand


mapContext : (Context model msg -> Context model msg) -> Cmd msg
mapContext mapper =
  Intention.toCmd "Elmer_MapContext" mapper

mapContextCommandRunner : CommandRunner model subMsg msg
mapContextCommandRunner command _ =
  let
    contextMapper = Intention.cmdValue command
  in
    CommandSuccess (applyToContext contextMapper)

applyToContext : (Context model msg -> Context model msg) -> Context model msg -> ( Context model msg, Cmd msg )
applyToContext mapper context =
  ( mapper context, Cmd.none )


generate : (Context model msg -> Cmd msg) -> Cmd msg
generate generator =
  Intention.toCmd "Elmer_Generate" generator

generateCommandRunner : CommandRunner model subMsg msg
generateCommandRunner command _ =
  let
    generator = Intention.cmdValue command
  in
    CommandSuccess (generateCommand generator)

generateCommand : (Context model msg -> Cmd msg) -> Context model msg -> ( Context model msg, Cmd msg )
generateCommand generator context =
  ( context, generator context )


fail : String -> Cmd msg
fail =
  Intention.toCmd "Elmer_Fail"

failCommandRunner : CommandRunner model subMsg msg
failCommandRunner command _ =
  let
    message = Intention.cmdValue command
  in
    CommandError message


stub : msg -> Cmd msg
stub =
  Intention.toCmd "Elmer_Stub"

stubCommandRunner : CommandRunner model subMsg msg
stubCommandRunner command tagger =
  let
    msg = Intention.cmdValue command
  in
    CommandSuccess (Context.update (tagger msg))


unknownCommandRunner : String -> CommandRunner model subMsg msg
unknownCommandRunner commandName _ _ =
  CommandError <|
    format <|
      [ message "Elmer encountered a command it does not know how to run" commandName
      , description "Try sending a stubbed or dummy command instead"
      ]
