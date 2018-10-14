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

import Elmer.Html.Types exposing (..)
import Elmer.Html.Printer as HtmlPrinter
import Elmer.Html.Query as Query
import Elmer.Html.Target as Target
import Elmer.Html.Selector as Selector
import Elmer.Html.Event.Description as EventDescription
import Elmer.Html.Event.Types exposing (..)
import Elmer.Html.Event.Processor exposing (processEvents, processEventsWhen)
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Printer exposing (..)
import Elmer


type ClickType
  = Single
  | Double


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
    eventDescriptions =
      [ EventDescription.forMouseEvent "click"
      , EventDescription.forMouseEvent "mousedown"
      , EventDescription.forMouseEvent "mouseup"
      , EventDescription.forSubmitEvent
      ]
  in
    case clickType of
      Single ->
        processEvents eventDescriptions testState
      Double ->
        processEvents
          ( eventDescriptions ++ [ EventDescription.forBasicEvent "dblclick" ] )
          testState


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
  processEvents
    [ EventDescription.forMouseEvent "mousedown"
    ]

{-| Trigger a mouse up event on the targeted element.
-}
release : Elmer.TestState model msg -> Elmer.TestState model msg
release =
  processEvents
    [ EventDescription.forMouseEvent "mouseup"
    ]

{-| Simulate moving the mouse into the targeted element.

This may trigger any relevant `mouseOver` or `mouseEnter` event handlers.

Note: Mouse enter events do not propagate, so a mouse enter action will only
trigger an event handler that is registered by the targeted element.
-}
moveMouseIn : Elmer.TestState model msg -> Elmer.TestState model msg
moveMouseIn =
  processEvents
    [ EventDescription.forInertMouseEvent "mouseenter"
    , EventDescription.forMouseEvent "mouseover"
    ]

{-| Simulate moving the mouse out of the targeted element.

This may trigger any relevant `mouseOut` or `mouseLeave` event handlers.

Note: Mouse leave events do not propagate, so a mouse leave action will only
trigger an event handler that is registered by the targeted element.
-}
moveMouseOut : Elmer.TestState model msg -> Elmer.TestState model msg
moveMouseOut =
  processEvents
    [ EventDescription.forInertMouseEvent "mouseleave"
    , EventDescription.forMouseEvent "mouseout"
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
  processEvents [ EventDescription.forInputEventWith inputString ]

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
  processEvents [ EventDescription.forCheckEvent doCheck ]


{-| Trigger an input event on the targeted select element.

The argument specifies the option to select by its `value` property.
-}
select : String -> Elmer.TestState model msg -> Elmer.TestState model msg
select value =
  processEventsWhen [ EventDescription.forInputEventWith value ] (\result ->
    result
      |> Result.andThen isSelectable
      |> Result.andThen (hasOption value)
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
      Target.forElement (ElementWith [ Selector.tag "option" ]) element
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
            , message "These are the options" ( HtmlPrinter.toString element )
            ]

findOption : String -> HtmlElement msg -> Maybe (HtmlElement msg)
findOption value element =
  element
    |> Target.forElement (ElementWith [ Selector.tag "option", Selector.attribute ("value", value) ])
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
  processEvents [ EventDescription.forEvent eventName eventJson ]

-- Private functions

processBasicEvent : String -> TestState model msg -> TestState model msg
processBasicEvent eventName =
  processEvents [ EventDescription.forBasicEvent eventName ]
