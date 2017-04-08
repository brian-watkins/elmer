module Elmer.Html.Event
    exposing
        ( click
        , doubleClick
        , press
        , release
        , moveMouseIn
        , moveMouseOut
        , focus
        , blur
        , input
        , check
        , uncheck
        , select
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

The `mouseEnter` and `mouseLeave` events are exceptions to this rule. These events only
trigger a handler attached to the targeted element. See `moveMouseIn` and `moveMouseOut`
for more.

# Mouse Events
@docs click, doubleClick, press, release, moveMouseIn, moveMouseOut

# Form Events
@docs input, check, uncheck, select

# Focus Events
@docs focus, blur

# Custom Events
@docs trigger

-}

import Json.Decode as Json
import Elmer.Html.Types exposing (..)
import Elmer.Html.Internal as HtmlInternal
import Elmer.Html.Query as Query
import Elmer.Html
import Elmer.Internal as Internal exposing (..)
import Elmer
import Elmer.Runtime as Runtime
import Elmer.Printer exposing (..)
import Dict
import Html exposing (Html)


type alias EventHandlerQuery msg =
  HtmlElement msg -> List (HtmlEventHandler msg)

type alias EventHandler msg =
  EventJson -> EventResult msg

type alias EventResult msg =
  Result String msg

type alias EventJson =
  String

type alias EventPropagation msg =
  { handlerQuery : EventHandlerQuery msg
  , event : EventJson
  }

type ClickType
  = Single
  | Double

eventPropagation : EventHandlerQuery msg -> EventJson -> EventPropagation msg
eventPropagation query event =
  { handlerQuery = query
  , event = event
  }

{-| Simulate a click on the targeted element.

A click will trigger the appropriate `click`, `mouseDown`, and `mouseUp` event
handlers on the targeted element or its ancestors.

A click on an input element with type submit or a button element with type submit (or
a button with no type specified) will also trigger the appropriate `submit` event handlers as follows:
- If the targeted element has a form attribute, then the submit handler
on the specified form will be triggered; if the specified form does not exist, no submit
handlers will be triggered.
- If the targeted element has no form attribute, then the submit handler on any form that is an
ancestor of the targeted element will be triggered.
-}
click : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
click =
  triggerClick Single

triggerClick : ClickType -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
triggerClick clickType componentState =
  let
    eventPropagations =
      [ basicEventPropagation "click"
      , basicEventPropagation "mousedown"
      , basicEventPropagation "mouseup"
      , eventPropagation (submitHandlerQuery (viewForState componentState)) "{}"
      ]
  in
    case clickType of
      Single ->
        updateComponentState eventPropagations componentState
      Double ->
        updateComponentState
          ( eventPropagations ++ [ basicEventPropagation "dblclick" ] )
          componentState

viewForState : ComponentState model msg -> Maybe (Html msg)
viewForState componentState =
  case componentState of
    Ready component ->
      Just <| component.view component.model
    Failed _ ->
      Nothing

submitHandlerQuery : Maybe (Html msg) -> EventHandlerQuery msg
submitHandlerQuery maybeDom element =
  if triggersSubmit element then
    Maybe.withDefault [] <|
      ( maybeDom |> Maybe.map (\dom ->
          case HtmlInternal.attribute "form" element of
            Just formId ->
              case formFor formId dom of
                Just formElement ->
                  elementEventHandlerQuery "submit" formElement
                Nothing ->
                  []
            Nothing ->
              eventHandlerQuery "submit" element
          )
      )
  else
    []

formFor : String -> Html msg -> Maybe (HtmlElement msg)
formFor formId dom =
  Query.findElement ("#" ++ formId) dom

triggersSubmit : HtmlElement msg -> Bool
triggersSubmit element =
  HtmlInternal.isSubmitInput element || HtmlInternal.isSubmitButton element


{-| Simulate a double click on the targeted element.

Two clicks will occur in succession, with the second also triggering a double
click event. See `click` above for a list of the events triggered by a click.
-}
doubleClick : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
doubleClick =
  triggerClick Single
    >> triggerClick Double

{-| Trigger a mouse down event on the targeted element.
-}
press : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
press =
    processBasicEvent "mousedown"

{-| Trigger a mouse up event on the targeted element.
-}
release : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
release =
    processBasicEvent "mouseup"

{-| Simulate moving the mouse into the targeted element.

This may trigger any relevant `mouseOver` or `mouseEnter` event handlers.

Note: Mouse enter events do not propagate, so a mouse enter action will only
trigger an event handler that is registered by the targeted element.
-}
moveMouseIn : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
moveMouseIn =
  updateComponentState
    [ basicElementEventPropagation "mouseenter"
    , basicEventPropagation "mouseover"
    ]

{-| Simulate moving the mouse out of the targeted element.

This may trigger any relevant `mouseOut` or `mouseLeave` event handlers.

Note: Mouse leave events do not propagate, so a mouse leave action will only
trigger an event handler that is registered by the targeted element.
-}
moveMouseOut : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
moveMouseOut =
  updateComponentState
    [ basicElementEventPropagation "mouseleave"
    , basicEventPropagation "mouseout"
    ]

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

{-| Trigger an input event on the targeted element.
-}
input : String -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
input inputString =
    trigger "input" (inputEvent inputString)

inputEvent : String -> EventJson
inputEvent value =
  "{\"target\":{\"value\":\"" ++ value ++ "\"}}"

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

{-| Trigger an input event on the targeted select element.

The argument specifies the option to select by its `value` property.
-}
select : String -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
select value =
  Internal.map (\component ->
    let
      eventPropagations = [ eventPropagation (eventHandlerQuery "input") (inputEvent value) ]
    in
      targetedElement component
        |> Result.andThen isSelectable
        |> Result.andThen (hasOption value)
        |> Result.andThen (hasHandlersFor eventPropagations)
        |> Result.andThen (apply eventPropagations component)
        |> toComponentState
  )

isSelectable : HtmlElement msg -> Result String (HtmlElement msg)
isSelectable element =
  if element.tag == "select" then
    Ok element
  else
    Err "The targeted element is not selectable"

hasOption : String -> HtmlElement msg -> Result String (HtmlElement msg)
hasOption value element =
  let
    options = Elmer.Html.findChildren "option" element
  in
    if List.isEmpty options then
      Err <| format [ message "No option found with value" value ]
    else
      case findOption value element of
        Just option ->
          Ok element
        Nothing ->
          Err <| format
            [ message "No option found with value" value
            , message "These are the options" ( HtmlInternal.toString element )
            ]

findOption : String -> HtmlElement msg -> Maybe (HtmlElement msg)
findOption value element =
  Elmer.Html.findChildren ("option[value='" ++ value ++ "']") element
    |> List.head


{-| Trigger a custom event on the targeted element. Provide the name of the event
and the proper representation of the event object in JSON format.

The following will trigger a `keyup` event:

    componentState
      |> trigger "keyup" "{\"keyCode\":65}"
-}
trigger : String -> EventJson -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
trigger eventName eventJson =
  updateComponentState [ eventPropagation (eventHandlerQuery eventName) eventJson ]

-- Private functions

processBasicEvent : String -> ComponentState model msg -> ComponentState model msg
processBasicEvent eventName =
  trigger eventName "{}"

basicEventPropagation : String -> EventPropagation msg
basicEventPropagation eventName =
  eventPropagation (eventHandlerQuery eventName) "{}"

basicElementEventPropagation : String -> EventPropagation msg
basicElementEventPropagation eventName =
  eventPropagation (elementEventHandlerQuery eventName) "{}"

processBasicElementEvent : String -> ComponentState model msg -> ComponentState model msg
processBasicElementEvent eventName =
  updateComponentState [ basicElementEventPropagation eventName ]

eventHandlerQuery : String -> EventHandlerQuery msg
eventHandlerQuery eventName element =
  List.append element.eventHandlers element.inheritedEventHandlers
    |> List.filter (\e -> e.eventType == eventName)

elementEventHandlerQuery : String -> EventHandlerQuery msg
elementEventHandlerQuery eventName element =
  List.filter (\e -> e.eventType == eventName) element.eventHandlers

updateComponentState : List (EventPropagation msg) -> ComponentState model msg -> ComponentState model msg
updateComponentState eventPropagations =
  Internal.map (\component ->
    targetedElement component
      |> Result.andThen (hasHandlersFor eventPropagations)
      |> Result.andThen (apply eventPropagations component)
      |> toComponentState
  )

hasHandlersFor : List (EventPropagation msg) -> HtmlElement msg -> Result String (HtmlElement msg)
hasHandlersFor eventPropagations element =
  let
    handlers = List.map (\ep -> ep.handlerQuery element) eventPropagations
      |> List.concat
  in
    if List.isEmpty handlers then
      Err <| "No relevant event handler found"
    else
      Ok element

apply : List (EventPropagation msg) -> Component model msg -> HtmlElement msg -> Result String (Component model msg)
apply eventPropagationList component element =
  List.foldl (\ep result ->
    case result of
      Ok component ->
        collectEventHandlers ep.handlerQuery element
          |> propagateEvent ep.event component
      Err _ ->
        result
  ) (Ok component) eventPropagationList

targetedElement : Component model msg -> Result String (HtmlElement msg)
targetedElement component =
  case Query.targetedElement component of
    Just element ->
      Ok element
    Nothing ->
      Err "No target element specified"

prepareHandler : HtmlEventHandler msg -> EventHandler msg
prepareHandler eventHandler =
  Json.decodeString eventHandler.decoder

collectEventHandlers : EventHandlerQuery msg -> HtmlElement msg -> List (EventHandler msg)
collectEventHandlers eventHandlerQuery element =
  eventHandlerQuery element
    |> takeUpTo (\handler -> handler.options.stopPropagation)
    |> List.map prepareHandler

propagateEvent : EventJson -> Component model msg -> List (EventHandler msg) -> Result String (Component model msg)
propagateEvent event component eventHandlers =
  List.foldl (\eventHandler componentResult ->
    case componentResult of
      Ok component ->
        updateComponent (eventHandler event) component
      Err _ ->
        componentResult
  ) (Ok component) eventHandlers

updateComponent : EventResult msg -> Component model msg -> Result String (Component model msg)
updateComponent result component =
  Result.andThen (\msg -> Runtime.performUpdate msg component) result

toComponentState : Result String (Component model msg) -> ComponentState model msg
toComponentState componentResult =
  case componentResult of
    Ok component ->
      Ready component
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
