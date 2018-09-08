module Elmer.Html.Event.HandlerQuery exposing
  ( bubbling
  , inert
  , forSubmitEvent
  )

import Elmer.Html.Event.Types exposing (..)
import Elmer.Html.Types exposing (..)
import Elmer.Html.Internal as HtmlInternal
import Elmer.Html.Query as Query
import Elmer.Html.Target as Target
import Elmer.Html.Selector as Selector
import Html exposing (Html)


bubbling : String -> EventHandlerQuery msg
bubbling eventType _ element =
  List.append element.eventHandlers element.inheritedEventHandlers
    |> filterByEventType eventType


inert : String -> EventHandlerQuery msg
inert eventType _ element =
  element.eventHandlers
    |> filterByEventType eventType


forSubmitEvent : EventHandlerQuery msg
forSubmitEvent view element =
  if triggersSubmit element then
    case HtmlInternal.attribute "form" element of
      Just formId ->
        case formFor formId view of
          Just formElement ->
            inert "submit" view formElement
          Nothing ->
            []
      Nothing ->
        bubbling "submit" view element
  else
    []


triggersSubmit : HtmlElement msg -> Bool
triggersSubmit element =
  HtmlInternal.isSubmitInput element || HtmlInternal.isSubmitButton element


formFor : String -> Html msg -> Maybe (HtmlElement msg)
formFor formId html =
  Target.forHtml (Batch [ Selector.id formId ]) html
    |> Query.findElement
    |> Result.toMaybe


filterByEventType : String -> List (HtmlEventHandler msg) -> List (HtmlEventHandler msg)
filterByEventType eventType =
  List.filter (\e -> e.eventType == eventType)
