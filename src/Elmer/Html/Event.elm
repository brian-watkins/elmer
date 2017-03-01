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
        , check
        , uncheck
        , on
        , trigger
        )

{-| Trigger events on targeted elements. When an event occurs, Elmer will
call the component's `update` method with the resulting message.

Elmer respects the `stopPropagation` option on event handlers. An event on an
element will bubble up through that element's ancestors until an event handler
says otherwise.

This means, for example, that you can `click` a targeted element whose
ancestor registers for click events and the event will be handled by that
ancestor as expected.

`mouseEnter` and `mouseLeave` are exceptions to this rule. These events only
trigger a handler attached to the targeted element. 

# Mouse Events
@docs click, doubleClick, mouseDown, mouseUp, mouseEnter, mouseLeave, mouseOver, mouseOut

# Form Events
@docs input, check, uncheck

# Focus Events
@docs focus, blur

# Custom Events
@docs on, trigger

-}

import Json.Decode as Json
import Elmer.Html.Types exposing (..)
import Elmer.Html.Internal as HtmlInternal
import Elmer.Html.Query as Query
import Elmer.Internal as Internal exposing (..)
import Elmer
import Elmer.Runtime as Runtime
import Dict


type alias EventHandler msg =
    HtmlEvent msg -> EventResult msg

type alias EventGenerator msg =
  HtmlElement msg -> Result String (List (HtmlEvent msg))

type alias EventResult msg =
  Result String msg


{-| Trigger a click event on the targeted element.
-}
click : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
click =
  processBasicEvent "click"

{-| Trigger a double click event on the targeted element.
-}
doubleClick : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
doubleClick =
    processBasicEvent "dblclick"

{-| Trigger a mouse down event on the targeted element.
-}
mouseDown : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseDown =
    processBasicEvent "mousedown"

{-| Trigger a mouse up event on the targeted element.
-}
mouseUp : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseUp =
    processBasicEvent "mouseup"

{-| Trigger a mouse enter event on the targeted element.

Note: Mouse enter events do not propagate, so a mouse enter action will only
trigger an event handler that is registered by the targeted element.
-}
mouseEnter : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseEnter =
  processBasicElementEvent "mouseenter"

{-| Trigger a mouse leave event on the targeted element.

Note: Mouse leave events do not propagate, so a mouse leave action will only
trigger an event handler that is registered by the targeted element.
-}
mouseLeave : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseLeave =
  processBasicElementEvent "mouseleave"

{-| Trigger a mouse over event on the targeted element.
-}
mouseOver : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseOver =
  processBasicEvent "mouseover"

{-| Trigger a mouse out event on the targeted element.
-}
mouseOut : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
mouseOut =
  processBasicEvent "mouseout"

{-| Trigger a focus event on the targeted element.
-}
focus : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
focus =
  processBasicEvent "focus"

{-| Trigger a blur event on the targeted element.
-}
blur : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
blur =
  processBasicEvent "blur"

processBasicEvent : String -> ComponentState model msg -> ComponentState model msg
processBasicEvent eventName =
  trigger eventName "{}"

processBasicElementEvent : String -> ComponentState model msg -> ComponentState model msg
processBasicElementEvent eventName =
  processEvents (gatherEventsOnElement eventName) <| genericHandler "{}"


{-| Trigger an input event on the targeted element.
-}
input : String -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
input inputString =
  let
      eventJson =
          "{\"target\":{\"value\":\"" ++ inputString ++ "\"}}"
  in
    trigger "input" eventJson

{-| Trigger a change event on the targeted checkbox element with
`True` for the `checked` property.
-}
check : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
check =
  handleCheck True

{-| Trigger a change event on the targeted checkbox element with
`False` for the `checked` property.
-}
uncheck : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
uncheck =
  handleCheck False

handleCheck : Bool -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
handleCheck doCheck =
  let
    eventJson = "{\"target\":{\"checked\":"
          ++ Internal.boolToString doCheck
          ++ "}}"
  in
    trigger "change" eventJson

{-| Deprecated. Use `trigger` instead.
-}
on : String -> String -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
on =
  trigger

{-| Trigger a custom event on the targeted element. Provide the name of the event
and the proper representation of the event object in JSON format.

The following will trigger a `keyup` event:

    componentState
      |> trigger "keyup" "{\"keyCode\":65}"
-}
trigger : String -> String -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
trigger eventName eventJson =
  processEvents (gatherEvents eventName) <| genericHandler eventJson


-- Private functions

gatherEvents : String -> EventGenerator msg
gatherEvents eventName element =
  let
    allEvents = List.append element.events element.inheritedEvents
    matchingEvents = List.filter (\e -> e.eventType == eventName) allEvents
  in
    if List.isEmpty matchingEvents then
      Err <| "No " ++ eventName ++ " event found on the targeted element or its ancestors"
    else
      Ok matchingEvents

gatherEventsOnElement : String -> EventGenerator msg
gatherEventsOnElement eventName element =
  let
    matchingEvents = List.filter (\e -> e.eventType == eventName) element.events
  in
    if List.isEmpty matchingEvents then
      Err <| "No " ++ eventName ++ " event found on the targeted element"
    else
      Ok matchingEvents

genericHandler : String -> HtmlEvent msg -> EventResult msg
genericHandler eventJson event =
  Json.decodeString event.decoder eventJson

processEvents : EventGenerator msg -> EventHandler msg -> ComponentState model msg -> ComponentState model msg
processEvents eventGenerator eventHandler =
  Internal.map
    <| mapTargetElement
    <| collectEvents eventGenerator
    <| propagateEvents eventHandler

mapTargetElement : (HtmlElement msg -> Component model msg -> ComponentState model msg) -> Component model msg -> ComponentState model msg
mapTargetElement mapper component =
  case Query.targetElement component of
    Just element ->
      mapper element component
    Nothing ->
      Failed "No target element specified"

collectEvents : EventGenerator msg -> (List (HtmlEvent msg) -> Component model msg -> ComponentState model msg) -> HtmlElement msg -> Component model msg -> ComponentState model msg
collectEvents eventGenerator operator element component =
  case eventGenerator element of
    Ok events ->
      let
        propagatingEvents = takeUpTo (\event -> event.options.stopPropagation) events
      in
        operator propagatingEvents component
    Err message ->
      Failed message

propagateEvents : EventHandler msg -> List (HtmlEvent msg) -> Component model msg -> ComponentState model msg
propagateEvents eventHandler events component =
  List.foldl (\event componentState ->
    case componentState of
      Ready component ->
        updateComponent (eventHandler event) component
      Failed _ ->
        componentState
  ) (Ready component) events

updateComponent : EventResult msg -> Component model msg -> ComponentState model msg
updateComponent result component =
  case result of
    Ok msg ->
      Runtime.performUpdate msg component
        |> asComponentState

    Err msg ->
        Failed msg

asComponentState : Result String (Component model msg) -> ComponentState model msg
asComponentState commandResult =
  case commandResult of
    Ok updatedComponentState ->
      Ready updatedComponentState
    Err message ->
      Failed message


takeUpTo : (a -> Bool) -> List a -> List a
takeUpTo predicate elements =
  case elements of
    [] ->
      []
    x :: xs ->
      if not <| predicate x then
        x :: takeUpTo predicate xs
      else
        [ x ]
