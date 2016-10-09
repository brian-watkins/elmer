module Elmer.Event exposing (click, input, on)

import Elmer.Shared exposing (..)
import Elmer.Types exposing (..)

getEvent : String -> HtmlNode -> Maybe HtmlEvent
getEvent eventName node =
  List.head (List.filter (\e -> e.eventType == eventName) node.events)

clickHandler : HtmlNode -> EventResult msg
clickHandler node =
  genericHandler "click" "{}" node

click : ComponentStateResult model msg -> ComponentStateResult model msg
click componentStateResult =
  handleEvent clickHandler componentStateResult

inputHandler : String -> HtmlNode -> EventResult msg
inputHandler inputString node =
  let
    eventJson = "{\"target\":{\"value\":\"" ++ inputString ++ "\"}}"
  in
    genericHandler "input" eventJson node

input : String -> ComponentStateResult model msg -> ComponentStateResult model msg
input inputString componentStateResult =
  handleEvent (inputHandler inputString) componentStateResult

genericHandler : String -> String -> HtmlNode -> EventResult msg
genericHandler eventName eventJson node =
  case getEvent eventName node of
    Just customEvent ->
      eventResult (Native.Helpers.getMessageForEvent customEvent.decoder eventJson)
    Nothing ->
      EventFailure ("No " ++ eventName ++ " event found")

on : String -> String -> ComponentStateResult model msg -> ComponentStateResult model msg
on eventName eventJson componentStateResult =
  handleEvent (genericHandler eventName eventJson) componentStateResult

-- Private functions

handleEvent : (HtmlNode -> EventResult msg) -> ComponentStateResult model msg -> ComponentStateResult model msg
handleEvent eventHandler componentStateResult =
  componentStateOrFail componentStateResult <|
    handleNodeEvent eventHandler

handleNodeEvent : (HtmlNode -> EventResult msg) -> HtmlComponentState model msg -> ComponentStateResult model msg
handleNodeEvent eventHandler componentState =
  case componentState.targetNode of
    Just node ->
      updateComponent node eventHandler componentState
    Nothing ->
      UpstreamFailure "No target node specified"

updateComponent : HtmlNode -> (HtmlNode -> EventResult msg) -> HtmlComponentState model msg -> ComponentStateResult model msg
updateComponent node eventHandler componentState =
  case eventHandler node of
    Message msg ->
      let
        updatedState = performUpdate msg componentState
      in
        CurrentState updatedState
    EventFailure msg ->
      UpstreamFailure msg

eventResult : Result String msg -> EventResult msg
eventResult eventResult =
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
