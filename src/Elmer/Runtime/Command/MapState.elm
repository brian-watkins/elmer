module Elmer.Runtime.Command.MapState exposing
  ( commandRunner
  )

import Elmer.Runtime.Types exposing (..)
import Elmer.Context as Context exposing (Context)


commandRunner : CommandRunner model subMsg msg
commandRunner command tagger =
  CommandSuccess (storeStateCommand <| Cmd.map tagger command)


storeStateCommand : Cmd msg -> Context model msg -> ( Context model msg, Cmd msg )
storeStateCommand command context =
  ( Context.updateState command context
  , Cmd.none
  )
