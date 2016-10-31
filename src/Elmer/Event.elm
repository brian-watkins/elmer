module Elmer.Event exposing (click, input, on, command)

import Elmer exposing (..)

type alias EventHandler msg =
  HtmlNode -> EventResult msg

type EventResult msg =
  Message msg |
  EventFailure String

clickHandler : EventHandler msg
clickHandler node =
  genericHandler "click" "{}" node

click : ComponentStateResult model msg -> ComponentStateResult model msg
click componentStateResult =
  handleEvent clickHandler componentStateResult

inputHandler : String -> EventHandler msg
inputHandler inputString node =
  let
    eventJson = "{\"target\":{\"value\":\"" ++ inputString ++ "\"}}"
  in
    genericHandler "input" eventJson node

input : String -> ComponentStateResult model msg -> ComponentStateResult model msg
input inputString componentStateResult =
  handleEvent (inputHandler inputString) componentStateResult

genericHandler : String -> String -> EventHandler msg
genericHandler eventName eventJson node =
  case getEvent eventName node of
    Just customEvent ->
      eventResult (Native.Helpers.getMessageForEvent customEvent.decoder eventJson)
    Nothing ->
      EventFailure ("No " ++ eventName ++ " event found")

on : String -> String -> ComponentStateResult model msg -> ComponentStateResult model msg
on eventName eventJson componentStateResult =
  handleEvent (genericHandler eventName eventJson) componentStateResult

command : msg -> ComponentStateResult model msg -> ComponentStateResult model msg
command message componentStateResult =
  componentStateResult
    |> Elmer.map (\state -> CurrentState (performUpdate message state))

-- Private functions

getEvent : String -> HtmlNode -> Maybe HtmlEvent
getEvent eventName node =
  List.head (List.filter (\e -> e.eventType == eventName) node.events)

handleEvent : EventHandler msg -> ComponentStateResult model msg -> ComponentStateResult model msg
handleEvent eventHandler componentStateResult =
  componentStateResult
    |> Elmer.map (handleNodeEvent eventHandler)

handleNodeEvent : EventHandler msg -> HtmlComponentState model msg -> ComponentStateResult model msg
handleNodeEvent eventHandler componentState =
  case componentState.targetNode of
    Just node ->
      updateComponent node eventHandler componentState
    Nothing ->
      UpstreamFailure "No target node specified"

updateComponent : HtmlNode -> EventHandler msg -> HtmlComponentState model msg -> ComponentStateResult model msg
updateComponent node eventHandler componentState =
  case eventHandler node of
    Message msg ->
      CurrentState (performUpdate msg componentState)
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
  in
    if command == Cmd.none then
      updatedState
    else
      let
        maybeUpdatedMessage = Native.Helpers.runCommand command
      in
        case maybeUpdatedMessage of
          Just updatedMessage ->
            performUpdate updatedMessage updatedState
          Nothing ->
            updatedState
