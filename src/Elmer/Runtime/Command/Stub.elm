module Elmer.Runtime.Command.Stub exposing
  ( with
  , commandRunner
  , name
  )

import Elmer.Runtime.Types exposing (..)
import Elmer.Runtime.Intention as Intention
import Elmer.Context as Context exposing (Context)


name : String
name =
  "Elmer_Stub"


with : msg -> Cmd msg
with =
  Intention.toCmd name


commandRunner : CommandRunner model subMsg msg
commandRunner command tagger =
  let
    msg = Intention.cmdValue command
  in
    CommandSuccess (Context.update (tagger msg))
