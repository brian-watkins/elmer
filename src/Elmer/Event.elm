module Elmer.Event
    exposing
        ( click
        , input
        , on
        , sendCommand
        )

import Navigation exposing (..)
import Json.Decode as Json
import Elmer.Types exposing (..)
import Elmer
import Elmer.Runtime as Runtime
import Dict


type alias EventHandler msg =
    HtmlNode msg -> EventResult msg


type EventResult msg
    = Message msg
    | EventFailure String


clickHandler : EventHandler msg
clickHandler node =
    genericHandler "click" "{}" node


click : ComponentStateResult model msg -> ComponentStateResult model msg
click componentStateResult =
    handleEvent clickHandler componentStateResult


inputHandler : String -> EventHandler msg
inputHandler inputString node =
    let
        eventJson =
            "{\"target\":{\"value\":\"" ++ inputString ++ "\"}}"
    in
        genericHandler "input" eventJson node


input : String -> ComponentStateResult model msg -> ComponentStateResult model msg
input inputString componentStateResult =
    handleEvent (inputHandler inputString) componentStateResult


genericHandler : String -> String -> EventHandler msg
genericHandler eventName eventJson node =
    case getEvent eventName node of
        Just customEvent ->
            let
                message =
                    Json.decodeString customEvent.decoder eventJson
            in
                eventResult message customEvent

        Nothing ->
            EventFailure ("No " ++ eventName ++ " event found")


on : String -> String -> ComponentStateResult model msg -> ComponentStateResult model msg
on eventName eventJson componentStateResult =
    handleEvent (genericHandler eventName eventJson) componentStateResult


sendCommand : Cmd msg -> ComponentStateResult model msg -> ComponentStateResult model msg
sendCommand command =
    Elmer.map (\state ->
      Runtime.performCommand command state
        |> asComponentStateResult
    )


-- Private functions

asComponentStateResult : Result String (HtmlComponentState model msg) -> ComponentStateResult model msg
asComponentStateResult commandResult =
  case commandResult of
    Ok updatedComponentState ->
      CurrentState updatedComponentState
    Err message ->
      UpstreamFailure message


getEvent : String -> HtmlNode msg -> Maybe (HtmlEvent msg)
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


updateComponent : HtmlNode msg -> EventHandler msg -> HtmlComponentState model msg -> ComponentStateResult model msg
updateComponent node eventHandler componentState =
    case eventHandler node of
        Message msg ->
          Runtime.performUpdate msg componentState
            |> asComponentStateResult

        EventFailure msg ->
            UpstreamFailure msg


eventResult : Result String msg -> HtmlEvent msg -> EventResult msg
eventResult eventResult event =
    case eventResult of
        Ok m ->
            Message m

        Err e ->
            EventFailure e
