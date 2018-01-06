module Elmer.Runtime.Command.Fail exposing
  ( commandRunner
  )

import Elmer.Runtime.Types exposing (..)
import Elmer.Runtime.Intention as Intention
import Elmer.Context as Context exposing (Context)


commandRunner : CommandRunner model subMsg msg
commandRunner command _ =
  let
    message = Intention.cmdValue command
  in
    CommandError message
