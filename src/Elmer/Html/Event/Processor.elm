module Elmer.Html.Event.Processor exposing
  ( processEvents
  , processEventsWhen
  )

import Elmer.Html.Event.Types exposing (..)
import Elmer.Html.Types exposing (..)
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Context.Internal as Context exposing (Context)
import Elmer.Runtime as Runtime
import Json.Decode as Json
import Elmer.Html.Query as Query


processEventsWhen : List (EventDescription msg) -> (Result String (HtmlElement msg) -> Result String (HtmlElement msg)) -> TestState model msg -> TestState model msg
processEventsWhen eventDescriptions assertions =
  TestState.map (\context ->
    targetedElement context
      |> assertions
      |> Result.andThen (hasHandlersFor eventDescriptions context)
      |> Result.andThen (apply eventDescriptions context)
      |> toTestState
  )


processEvents : List (EventDescription msg) -> TestState model msg -> TestState model msg
processEvents eventDescriptions =
  processEventsWhen eventDescriptions identity


hasHandlersFor : List (EventDescription msg) -> Context model msg -> HtmlElement msg -> Result String (HtmlElement msg)
hasHandlersFor eventDescriptions context element =
  let
    handlers = List.map (\ed -> ed.handlers (Context.render context) element) eventDescriptions
      |> List.concat
  in
    if List.isEmpty handlers then
      Err <| "No event handlers found for any of the triggered events: " ++ (
        List.map .eventType eventDescriptions
          |> String.join ", "
      )
    else
      Ok element


apply : List (EventDescription msg) -> Context model msg -> HtmlElement msg -> Result String (Context model msg)
apply eventDescriptionList context element =
  List.foldl (\evtDescr result ->
    case result of
      Ok context ->
        collectEventHandlers evtDescr.handlers context element
          |> bubbleEvent evtDescr.eventJson context
      Err _ ->
        result
  ) (Ok context) eventDescriptionList


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


bubbleEvent : EventJson -> Context model msg -> List (EventHandler msg) -> Result String (Context model msg)
bubbleEvent event context eventHandlers =
  List.foldl (\eventHandler contextResult ->
    case contextResult of
      Ok context ->
        updateContext (eventHandler event) context
      Err _ ->
        contextResult
  ) (Ok context) eventHandlers


updateContext : EventResult msg -> Context model msg -> Result String (Context model msg)
updateContext result context =
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
