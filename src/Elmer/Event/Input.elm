module Elmer.Event.Input exposing (inputResult)

import Elmer.Event.Shared exposing (..)
import Elmer.Types exposing (..)

inputResult : String -> SearchResult -> EventResult msg
inputResult inputString searchResult =
  passNodeOrFail searchResult (inputCaller inputString)

inputCaller : String -> HtmlNode -> EventResult msg
inputCaller inputString node =
  case node.events `Maybe.andThen` .input of
    Just inputEvent ->
      let
        eventJson = "{\"target\":{\"value\":\"" ++ inputString ++ "\"}}"
      in
        handleEventResult (Native.Helpers.getMessageForEvent inputEvent.decoder eventJson)
    Nothing ->
      EventFailure "No input event found"
