module Elmer.Runtime.Command.Defer exposing
  ( with
  , clear
  , fromContext
  )

import Elmer.Context as Context exposing (Context)
import Elmer.Runtime.Command.MapState as MapState


type DeferState
  = DeferredCommands


clear : Cmd msg
clear =
  MapState.with DeferredCommands <|
    \_ -> []


fromContext : Context model msg -> List (Cmd msg)
fromContext context =
  Context.state DeferredCommands context
    |> Maybe.withDefault []


with : Cmd msg -> Cmd msg
with command =
  MapState.with DeferredCommands <|
    updateStateWithDeferredCommand command


updateStateWithDeferredCommand : Cmd msg -> Maybe (List (Cmd msg)) -> List (Cmd msg)
updateStateWithDeferredCommand command state =
  Maybe.withDefault [] state
    |> (::) command
