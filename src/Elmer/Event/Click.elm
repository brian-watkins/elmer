module Elmer.Event.Click exposing (click, clickResult)

import Elmer.Event.Shared exposing (..)
import Elmer.Shared exposing (..)
import Elmer.Types exposing (..)

clickResult : SearchResult -> EventResult msg
clickResult searchResult =
  passNodeOrFail searchResult clickCaller

clickCaller : HtmlNode -> EventResult msg
clickCaller node =
  case node.events `Maybe.andThen` .click of
    Just clickEvent ->
      handleEventResult (Native.Helpers.getMessageForEvent clickEvent.decoder "{}")
    Nothing ->
      EventFailure "No click event found"

click : ComponentStateResult model msg -> ComponentStateResult model msg
click componentStateResult =
  componentStateOrFail componentStateResult (
    \componentState ->
      case componentState.targetNode of
        Just node ->
          case clickResult (Found node) of
            Message msg ->
              let
                updatedState = performUpdate msg componentState
              in
                CurrentState updatedState
            EventFailure msg ->
              UpstreamFailure msg
        Nothing ->
          UpstreamFailure "No target node specified"
  )
