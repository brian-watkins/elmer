module Elmer.Html.Types exposing
  ( HtmlState(..)
  , HtmlNode(..)
  , HtmlElement
  , HtmlEventHandler
  , HtmlFact(..)
  )

import Json.Decode as Json
import Dict exposing (Dict)


type HtmlState =
  TargetSelector

type HtmlNode msg
    = Element (HtmlElement msg)
    | Text String

type alias HtmlElement msg =
    { tag : String
    , facts : String
    , children : List (HtmlNode msg)
    , inheritedEventHandlers : List (HtmlEventHandler msg)
    , eventHandlers : List (HtmlEventHandler msg)
    }

type alias HtmlEventHandler msg =
    { eventType : String
    , options : EventHandlerOptions
    , decoder : Json.Decoder msg
    }

type alias EventHandlerOptions =
  { stopPropagation : Bool
  , preventDefault : Bool
  }

type HtmlFact
  = StringValue String
  | BoolValue Bool
  | DictValue (Dict String String)
