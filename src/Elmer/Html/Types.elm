module Elmer.Html.Types exposing
  ( HtmlNode(..)
  , HtmlElement
  , HtmlEvent
  , HtmlFact(..)
  )

import Json.Decode as Json
import Dict exposing (Dict)

type HtmlNode msg
    = Element (HtmlElement msg)
    | Text String

type alias HtmlElement msg =
    { tag : String
    , facts : String
    , children : List (HtmlNode msg)
    , inheritedEvents : List (HtmlEvent msg)
    , events : List (HtmlEvent msg)
    }

type alias HtmlEvent msg =
    { eventType : String
    , options : HtmlEventOptions
    , decoder : Json.Decoder msg
    }

type alias HtmlEventOptions =
  { stopPropagation : Bool
  , preventDefault : Bool
  }

type HtmlFact
  = StringValue String
  | BoolValue Bool
  | DictValue (Dict String String)
