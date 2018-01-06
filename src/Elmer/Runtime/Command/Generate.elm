module Elmer.Runtime.Command.Generate exposing
  ( commandRunner
  )

import Elmer.Runtime.Types exposing (..)
import Elmer.Runtime.Intention as Intention
import Elmer.Context as Context exposing (Context)


commandRunner : CommandRunner model subMsg msg
commandRunner command _ =
  let
    generator = Intention.cmdValue command
  in
    CommandSuccess (generateCommand generator)


generateCommand : (Context model msg -> Cmd msg) -> Context model msg -> ( Context model msg, Cmd msg )
generateCommand generator context =
  ( context, generator context )
