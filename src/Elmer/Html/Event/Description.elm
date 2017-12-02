module Elmer.Html.Event.Description exposing
  ( forMouseEvent
  , forInertMouseEvent
  , forSubmitEvent
  , forInputEventWith
  , forCheckEvent
  , forBasicEvent
  , forEvent
  )

import Elmer.Html.Event.Types exposing (..)
import Elmer.Html.Event.HandlerQuery as EventHandlerQuery
import Elmer.Internal as Internal
import Regex exposing (regex, HowMany(..))


create : String -> EventHandlerQuery msg -> EventJson -> EventDescription msg
create eventType query event =
  { handlers = query
  , eventJson = event
  , eventType = eventType
  }


forEvent : String -> EventJson -> EventDescription msg
forEvent eventName eventJson =
  eventJson
    |> create eventName (EventHandlerQuery.bubbling eventName)


forBasicEvent : String -> EventDescription msg
forBasicEvent eventName =
  basicEventJson
    |> forEvent eventName


forInputEventWith : String -> EventDescription msg
forInputEventWith value =
  inputEventJson value
    |> forEvent "input"


forCheckEvent : Bool -> EventDescription msg
forCheckEvent doCheck =
  checkEventJson doCheck
    |> forEvent "change"


forSubmitEvent : EventDescription msg
forSubmitEvent =
  basicEventJson
    |> create "submit" EventHandlerQuery.forSubmitEvent


forMouseEvent : String -> EventDescription msg
forMouseEvent eventName =
  mouseEventJson
    |> forEvent eventName


forInertMouseEvent : String -> EventDescription msg
forInertMouseEvent eventName =
  mouseEventJson
    |> create eventName (EventHandlerQuery.inert eventName)


basicEventJson : EventJson
basicEventJson =
  "{}"


mouseEventJson : String
mouseEventJson =
  "{\"pageX\":0,\"pageY\":0}"


checkEventJson : Bool -> EventJson
checkEventJson doCheck =
  Internal.boolToString doCheck
    |> withTemplate "{\"target\":{\"checked\":?}}"


inputEventJson : String -> EventJson
inputEventJson =
  withTemplate "{\"target\":{\"value\":\"?\"}}"


withTemplate : String -> String -> String
withTemplate template value =
  Regex.replace All (regex "[?]") (\_ -> value) template
