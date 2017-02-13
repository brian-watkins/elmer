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
    , events : List (HtmlEvent msg)
    }

type alias HtmlEvent msg =
    { eventType : String
    , decoder : Json.Decoder msg
    }

type HtmlFact
  = StringValue String
  | DictValue (Dict String String)
