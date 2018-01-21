module Elmer.Runtime.Command.MapState exposing
  ( with
  , commandRunner
  , name
  )

import Elmer.Runtime.Intention as Intention
import Elmer.Runtime.Types exposing (..)
import Elmer.Context as Context exposing (Context)


name : String
name =
  "Elmer_MapState"


with : typeId -> (Maybe a -> a) -> Cmd msg
with typeId mapper =
  Intention.toCmd name { typeId = typeId, mapper = mapper }


commandRunner : CommandRunner model subMsg msg
commandRunner command tagger =
  CommandSuccess (storeStateCommand <| Cmd.map tagger command)


storeStateCommand : Cmd msg -> Context model msg -> ( Context model msg, Cmd msg )
storeStateCommand command context =
  ( Context.updateState command context
  , Cmd.none
  )
