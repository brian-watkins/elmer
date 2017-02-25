module Elmer.Html.Event
    exposing
        ( click
        , doubleClick
        , mouseDown
        , mouseUp
        , mouseEnter
        , mouseLeave
        , input
        , on
        )

{-| Trigger events on targeted elements. When an event occurs, Elmer will
call the component's `update` method with the resulting message.

# Mouse Events
@docs click, doubleClick, mouseDown, mouseUp, mouseEnter, mouseLeave

# Form Events
@docs input

# Custom Events
@docs on

-}

import Json.Decode as Json
import Elmer.Html.Types exposing (..)
import Elmer.Internal as Internal exposing (..)
import Elmer
import Elmer.Runtime as Runtime
import Dict


type alias EventHandler msg =
    HtmlElement msg -> EventResult msg


type EventResult msg
    = Message msg
    | EventFailure String


basicHandler : String -> EventHandler msg
basicHandler eventType node =
    genericHandler eventType "{}" node

{-| Trigger a click event on the targeted element.
-}
click : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
click =
    handleEvent <| basicHandler "click"

{-| Trigger a double click event on the targeted element.
-}
doubleClick : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
doubleClick =
    handleEvent <| basicHandler "dblclick"

{-| Trigger a mouse down event on the targeted element.
-}
mouseDown : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseDown =
    handleEvent <| basicHandler "mousedown"

{-| Trigger a mouse up event on the targeted element.
-}
mouseUp : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseUp =
    handleEvent <| basicHandler "mouseup"

{-| Trigger a mouse enter event on the targeted element.
-}
mouseEnter : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseEnter =
    handleEvent <| basicHandler "mouseenter"

{-| Trigger a mouse leave event on the targeted element.
-}
mouseLeave : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseLeave =
    handleEvent <| basicHandler "mouseleave"


inputHandler : String -> EventHandler msg
inputHandler inputString node =
    let
        eventJson =
            "{\"target\":{\"value\":\"" ++ inputString ++ "\"}}"
    in
        genericHandler "input" eventJson node

{-| Trigger an input event on the targeted element.
-}
input : String -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
input inputString =
    handleEvent <| inputHandler inputString


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

{-| Trigger a custom event on the targeted element.

The following will trigger a `keyup` event:

    componentState
      |> on "keyup" "{\"keyCode\":65}"
-}
on : String -> String -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
on eventName eventJson =
    handleEvent <| genericHandler eventName eventJson


-- Private functions


getEvent : String -> HtmlElement msg -> Maybe (HtmlEvent msg)
getEvent eventName node =
    List.head (List.filter (\e -> e.eventType == eventName) node.events)


handleEvent : EventHandler msg -> ComponentState model msg -> ComponentState model msg
handleEvent eventHandler =
    Internal.map <| handleNodeEvent eventHandler


handleNodeEvent : EventHandler msg -> Component model msg -> ComponentState model msg
handleNodeEvent eventHandler componentState =
    case componentState.targetElement of
        Just node ->
            updateComponent node eventHandler componentState

        Nothing ->
            Failed "No target node specified"


updateComponent : HtmlElement msg -> EventHandler msg -> Component model msg -> ComponentState model msg
updateComponent node eventHandler componentState =
    case eventHandler node of
        Message msg ->
          Runtime.performUpdate msg componentState
            |> asComponentState

        EventFailure msg ->
            Failed msg


asComponentState : Result String (Component model msg) -> ComponentState model msg
asComponentState commandResult =
  case commandResult of
    Ok updatedComponentState ->
      Ready updatedComponentState
    Err message ->
      Failed message


eventResult : Result String msg -> HtmlEvent msg -> EventResult msg
eventResult eventResult event =
    case eventResult of
        Ok m ->
            Message m

        Err e ->
            EventFailure e
