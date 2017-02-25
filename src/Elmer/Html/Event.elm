module Elmer.Html.Event
    exposing
        ( click
        , doubleClick
        , mouseDown
        , mouseUp
        , mouseEnter
        , mouseLeave
        , mouseOver
        , mouseOut
        , focus
        , blur
        , input
        , on
        )

{-| Trigger events on targeted elements. When an event occurs, Elmer will
call the component's `update` method with the resulting message.

# Mouse Events
@docs click, doubleClick, mouseDown, mouseUp, mouseEnter, mouseLeave, mouseOver, mouseOut

# Form Events
@docs input

# Focus Events
@docs focus, blur

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


{-| Trigger a mouse over event on the targeted element.

Note: A mouse over event is typically triggered whenever the mouse moves
onto the element that has the event listener attached or one of its children.
To simulate this behavior, Elmer allows you to call `mouseOver` on any targeted
element so long as it or one of its ancestors registers to handle
mouse over events.
-}
mouseOver : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseOver =
    handleEvent <| basicInheritableHandler "mouseover"

{-| Trigger a mouse out event on the targeted element.

Note: A mouse out event is typically triggered whenever the mouse moves
out of the element that has the event listener attached or one of its children.
To simulate this behavior, Elmer allows you to call `mouseOut` on any targeted
element so long as it or one of its ancestors registers to handle
mouse out events.
-}
mouseOut : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseOut =
    handleEvent <| basicInheritableHandler "mouseout"

{-| Trigger a focus event on the targeted element.
-}
focus : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
focus =
  handleEvent <| basicHandler "focus"

{-| Trigger a blur event on the targeted element.
-}
blur : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
blur =
  handleEvent <| basicHandler "blur"

{-| Trigger an input event on the targeted element.
-}
input : String -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
input inputString =
    handleEvent <| inputHandler inputString


{-| Trigger a custom event on the targeted element.

The following will trigger a `keyup` event:

    componentState
      |> on "keyup" "{\"keyCode\":65}"
-}
on : String -> String -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
on eventName eventJson =
    handleEvent <| elementEventHandler eventName eventJson


-- Private functions

basicHandler : String -> EventHandler msg
basicHandler eventType node =
    elementEventHandler eventType "{}" node

basicInheritableHandler : String -> EventHandler msg
basicInheritableHandler eventType node =
    inheritedEventHandler eventType "{}" node

inputHandler : String -> EventHandler msg
inputHandler inputString =
    let
        eventJson =
            "{\"target\":{\"value\":\"" ++ inputString ++ "\"}}"
    in
        elementEventHandler "input" eventJson

elementEventHandler : String -> String -> EventHandler msg
elementEventHandler eventName eventJson node =
  case elementEvent eventName node of
    Just event ->
      genericHandler event eventJson
    Nothing ->
      EventFailure ("No " ++ eventName ++ " event found")

inheritedEventHandler : String -> String -> EventHandler msg
inheritedEventHandler eventName eventJson node =
  case inheritedEvent eventName node of
    Just event ->
      genericHandler event eventJson
    Nothing ->
      EventFailure ("No " ++ eventName ++ " event found on the targeted element or its ancestors")

genericHandler : HtmlEvent msg -> String -> EventResult msg
genericHandler event eventJson =
  let
    message = Json.decodeString event.decoder eventJson
  in
    eventResult message event

elementEvent : String -> HtmlElement msg -> Maybe (HtmlEvent msg)
elementEvent eventName node =
    List.head (List.filter (\e -> e.eventType == eventName) node.events)

inheritedEvent : String -> HtmlElement msg -> Maybe (HtmlEvent msg)
inheritedEvent eventName node =
  let
    allEvents = List.append node.events node.inheritedEvents
  in
    List.head (List.filter (\e -> e.eventType == eventName) allEvents)


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
