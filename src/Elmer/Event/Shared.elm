module Elmer.Event.Shared exposing (..)

import Elmer.Types exposing (..)


passNodeOrFail : SearchResult -> (HtmlNode -> EventResult msg) -> EventResult msg
passNodeOrFail searchResult eventCaller =
  case searchResult of
    Found node ->
      eventCaller node
    SearchFailure msg ->
      EventFailure msg

handleEventResult : Result String msg -> EventResult msg
handleEventResult eventResult =
  case eventResult of
    Ok m ->
      Message m
    Err e ->
      EventFailure e


performUpdate : msg -> HtmlComponentState model msg -> HtmlComponentState model msg
performUpdate message componentState =
  let
    (updatedModel, command) = componentState.update message componentState.model
    updatedState = { componentState | model = updatedModel }
    updatedMessage = Native.Helpers.runCommand command
  in
    if command == Cmd.none then
      updatedState
    else
      performUpdate updatedMessage updatedState
