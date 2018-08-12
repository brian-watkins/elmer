module Elmer.Html.Event.Types exposing
  ( EventHandlerQuery
  , EventHandler
  , EventResult
  , EventJson
  , EventDescription
  )

import Html exposing (Html)
import Elmer.Html.Types exposing (..)

type alias EventHandlerQuery msg =
  Html msg -> HtmlElement msg -> List (HtmlEventHandler msg)

type alias EventHandler msg =
  EventJson -> EventResult msg

type alias EventResult msg =
  Result String (HtmlEventValue msg)

type alias EventJson =
  String

type alias EventDescription msg =
  { handlers : EventHandlerQuery msg
  , eventJson : EventJson
  , eventType: String
  }
