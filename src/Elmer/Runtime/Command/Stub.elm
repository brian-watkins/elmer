module Elmer.Runtime.Command.Stub exposing
  ( commandRunner
  )

import Elmer.Runtime.Types exposing (..)
import Elmer.Runtime.Intention as Intention
import Elmer.Context as Context exposing (Context)


commandRunner : CommandRunner model subMsg msg
commandRunner command tagger =
  let
    msg = Intention.cmdValue command
  in
    CommandSuccess (Context.update (tagger msg))
