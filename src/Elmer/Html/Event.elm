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

All mouse events occur at a simulated position of `{ pageX = 0, pageY = 0}`. If your
test needs a mouse event to occur at a specific position, use `trigger`.

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
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Context.Internal as Context exposing (Context)
import Elmer.Internal as Internal
import Elmer
import Elmer.Runtime as Runtime
import Elmer.Printer exposing (..)
import Dict
import Html exposing (Html)


type alias EventHandlerQuery msg =
  Html msg -> HtmlElement msg -> List (HtmlEventHandler msg)

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
click : Elmer.TestState model msg -> Elmer.TestState model msg
click =
  triggerClick Single

triggerClick : ClickType -> Elmer.TestState model msg -> Elmer.TestState model msg
triggerClick clickType testState =
  let
    eventPropagations =
      [ mouseEventPropagation "click"
      , mouseEventPropagation "mousedown"
      , mouseEventPropagation "mouseup"
      , eventPropagation submitHandlerQuery "{}"
      ]
  in
    case clickType of
      Single ->
        updateTestState eventPropagations testState
      Double ->
        updateTestState
          ( eventPropagations ++ [ basicEventPropagation "dblclick" ] )
          testState

submitHandlerQuery : EventHandlerQuery msg
submitHandlerQuery view element =
  if triggersSubmit element then
    case HtmlInternal.attribute "form" element of
      Just formId ->
        case formFor formId view of
          Just formElement ->
            elementEventHandlerQuery "submit" view formElement
          Nothing ->
            []
      Nothing ->
        eventHandlerQuery "submit" view element
  else
    []

formFor : String -> Html msg -> Maybe (HtmlElement msg)
formFor formId html =
  Query.forHtml ("#" ++ formId) html
    |> Query.findElement
    |> Result.toMaybe

triggersSubmit : HtmlElement msg -> Bool
triggersSubmit element =
  HtmlInternal.isSubmitInput element || HtmlInternal.isSubmitButton element


{-| Simulate a double click on the targeted element.

Two clicks will occur in succession, with the second also triggering a double
click event. See `click` above for a list of the events triggered by a click.
-}
doubleClick : Elmer.TestState model msg -> Elmer.TestState model msg
doubleClick =
  triggerClick Single
    >> triggerClick Double

{-| Trigger a mouse down event on the targeted element.
-}
press : Elmer.TestState model msg -> Elmer.TestState model msg
press =
  updateTestState
    [ mouseEventPropagation "mousedown"
    ]

{-| Trigger a mouse up event on the targeted element.
-}
release : Elmer.TestState model msg -> Elmer.TestState model msg
release =
  updateTestState
    [ mouseEventPropagation "mouseup"
    ]

{-| Simulate moving the mouse into the targeted element.

This may trigger any relevant `mouseOver` or `mouseEnter` event handlers.

Note: Mouse enter events do not propagate, so a mouse enter action will only
trigger an event handler that is registered by the targeted element.
-}
moveMouseIn : Elmer.TestState model msg -> Elmer.TestState model msg
moveMouseIn =
  updateTestState
    [ mouseElementEventPropagation "mouseenter"
    , mouseEventPropagation "mouseover"
    ]

{-| Simulate moving the mouse out of the targeted element.

This may trigger any relevant `mouseOut` or `mouseLeave` event handlers.

Note: Mouse leave events do not propagate, so a mouse leave action will only
trigger an event handler that is registered by the targeted element.
-}
moveMouseOut : Elmer.TestState model msg -> Elmer.TestState model msg
moveMouseOut =
  updateTestState
    [ mouseElementEventPropagation "mouseleave"
    , mouseEventPropagation "mouseout"
    ]

{-| Trigger a focus event on the targeted element.
-}
focus : Elmer.TestState model msg -> Elmer.TestState model msg
focus =
  processBasicEvent "focus"

{-| Trigger a blur event on the targeted element.
-}
blur : Elmer.TestState model msg -> Elmer.TestState model msg
blur =
  processBasicEvent "blur"

{-| Trigger an input event on the targeted element.
-}
input : String -> Elmer.TestState model msg -> Elmer.TestState model msg
input inputString =
    trigger "input" (inputEvent inputString)

inputEvent : String -> EventJson
inputEvent value =
  "{\"target\":{\"value\":\"" ++ value ++ "\"}}"

{-| Trigger a change event on the targeted checkbox element with
`True` for the `checked` property.
-}
check : Elmer.TestState model msg -> Elmer.TestState model msg
check =
  handleCheck True

{-| Trigger a change event on the targeted checkbox element with
`False` for the `checked` property.
-}
uncheck : Elmer.TestState model msg -> Elmer.TestState model msg
uncheck =
  handleCheck False

handleCheck : Bool -> Elmer.TestState model msg -> Elmer.TestState model msg
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
select : String -> Elmer.TestState model msg -> Elmer.TestState model msg
select value =
  TestState.map (\context ->
    let
      eventPropagations = [ eventPropagation (eventHandlerQuery "input") (inputEvent value) ]
    in
      targetedElement context
        |> Result.andThen isSelectable
        |> Result.andThen (hasOption value)
        |> Result.andThen (hasHandlersFor context eventPropagations)
        |> Result.andThen (apply eventPropagations context)
        |> toTestState
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
    options =
      Query.forElement "option" element
        |> Query.findElements
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
  Query.forElement ("option[value='" ++ value ++ "']") element
    |> Query.findElements
    |> List.head


{-| Trigger a custom event on the targeted element. Provide the name of the event
and the proper representation of the event object in JSON format.

The following will trigger a `keyup` event:

    testState
      |> trigger "keyup" "{\"keyCode\":65}"
-}
trigger : String -> String -> Elmer.TestState model msg -> Elmer.TestState model msg
trigger eventName eventJson =
  updateTestState [ eventPropagation (eventHandlerQuery eventName) eventJson ]

-- Private functions

processBasicEvent : String -> TestState model msg -> TestState model msg
processBasicEvent eventName =
  trigger eventName "{}"

mouseEventJson : String
mouseEventJson =
  "{\"pageX\":0,\"pageY\":0}"

mouseEventPropagation : String -> EventPropagation msg
mouseEventPropagation eventName =
  eventPropagation (eventHandlerQuery eventName) mouseEventJson

basicEventPropagation : String -> EventPropagation msg
basicEventPropagation eventName =
  eventPropagation (eventHandlerQuery eventName) "{}"

mouseElementEventPropagation : String -> EventPropagation msg
mouseElementEventPropagation eventName =
  eventPropagation (elementEventHandlerQuery eventName) mouseEventJson

basicElementEventPropagation : String -> EventPropagation msg
basicElementEventPropagation eventName =
  eventPropagation (elementEventHandlerQuery eventName) "{}"

processBasicElementEvent : String -> TestState model msg -> TestState model msg
processBasicElementEvent eventName =
  updateTestState [ basicElementEventPropagation eventName ]

eventHandlerQuery : String -> EventHandlerQuery msg
eventHandlerQuery eventName _ element =
  List.append element.eventHandlers element.inheritedEventHandlers
    |> List.filter (\e -> e.eventType == eventName)

elementEventHandlerQuery : String -> EventHandlerQuery msg
elementEventHandlerQuery eventName _ element =
  List.filter (\e -> e.eventType == eventName) element.eventHandlers

updateTestState : List (EventPropagation msg) -> TestState model msg -> TestState model msg
updateTestState eventPropagations =
  TestState.map (\context ->
    targetedElement context
      |> Result.andThen (hasHandlersFor context eventPropagations)
      |> Result.andThen (apply eventPropagations context)
      |> toTestState
  )

hasHandlersFor : Context model msg -> List (EventPropagation msg) -> HtmlElement msg -> Result String (HtmlElement msg)
hasHandlersFor context eventPropagations element =
  let
    handlers = List.map (\ep -> ep.handlerQuery (Context.render context) element) eventPropagations
      |> List.concat
  in
    if List.isEmpty handlers then
      Err <| "No relevant event handler found"
    else
      Ok element

apply : List (EventPropagation msg) -> Context model msg -> HtmlElement msg -> Result String (Context model msg)
apply eventPropagationList context element =
  List.foldl (\ep result ->
    case result of
      Ok context ->
        collectEventHandlers ep.handlerQuery context element
          |> propagateEvent ep.event context
      Err _ ->
        result
  ) (Ok context) eventPropagationList

targetedElement : Context model msg -> Result String (HtmlElement msg)
targetedElement context =
  case context.targetSelector of
    Just selector ->
      Query.findElement <| Query.forHtml selector (Context.render context)
    Nothing ->
      Err "No element has been targeted. Use Elmer.Html.target to identify an element to receive the event."

prepareHandler : HtmlEventHandler msg -> EventHandler msg
prepareHandler eventHandler =
  Json.decodeString eventHandler.decoder

collectEventHandlers : EventHandlerQuery msg -> Context model msg -> HtmlElement msg -> List (EventHandler msg)
collectEventHandlers eventHandlerQuery context element =
  eventHandlerQuery (Context.render context) element
    |> takeUpTo (\handler -> handler.options.stopPropagation)
    |> List.map prepareHandler

propagateEvent : EventJson -> Context model msg -> List (EventHandler msg) -> Result String (Context model msg)
propagateEvent event context eventHandlers =
  List.foldl (\eventHandler contextResult ->
    case contextResult of
      Ok context ->
        updateComponent (eventHandler event) context
      Err _ ->
        contextResult
  ) (Ok context) eventHandlers

updateComponent : EventResult msg -> Context model msg -> Result String (Context model msg)
updateComponent result context =
  Result.andThen (\msg -> Runtime.performUpdate msg context) result

toTestState : Result String (Context model msg) -> TestState model msg
toTestState contextResult =
  case contextResult of
    Ok context ->
      TestState.with context
    Err message ->
      TestState.failure message


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
