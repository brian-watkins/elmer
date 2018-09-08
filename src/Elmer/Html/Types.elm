module Elmer.Html.Types exposing
  ( HtmlSelector
  , HtmlSelectorGroup(..)
  , HtmlState(..)
  , HtmlNode(..)
  , HtmlElement
  , HtmlEventHandler
  , HtmlEventValue
  , HtmlFact(..)
  , HtmlTarget(..)
  , Selection
  )

{-| Types for working with Html. Exposed for Testing only

@docs HtmlState, HtmlNode, HtmlElement, HtmlEventHandler, HtmlEventValue, HtmlFact, HtmlTarget, HtmlSelectorGroup, HtmlSelector, Selection

-}

import Json.Decode as Json
import Dict exposing (Dict)

{-| HtmlState
-}
type HtmlState =
  TargetSelector

{-| HtmlNode
-}
type HtmlNode msg
    = Element (HtmlElement msg)
    | Text String

{-| HtmlElement
-}
type alias HtmlElement msg =
    { tag : String
    , properties : Dict String HtmlFact
    , attributes : Dict String String
    , styles : Dict String String
    , children : List (HtmlNode msg)
    , inheritedEventHandlers : List (HtmlEventHandler msg)
    , eventHandlers : List (HtmlEventHandler msg)
    }

{-| HtmlEventHandler
-}
type alias HtmlEventHandler msg =
    { eventType : String
    , decoder : Json.Decoder (HtmlEventValue msg)
    }

{-|
-}
type alias HtmlEventValue msg =
  { message : msg 
  , stopPropagation : Bool
  , preventDefault : Bool
  }


{-| HtmlFact
-}
type HtmlFact
  = StringValue String
  | BoolValue Bool


{-| HtmlTarget
-}
type HtmlTarget msg =
  HtmlTarget (Selection msg)

{-|
-}
type alias Selection msg =
  { selector : HtmlSelectorGroup msg
  , element : Maybe (HtmlElement msg)
  }


{-| HtmlSelector
-}
type HtmlSelectorGroup msg
  = Batch (List (HtmlSelector msg))
  | Within (List (HtmlSelector msg)) (HtmlSelectorGroup msg)


{-|
-}
type alias HtmlSelector msg =
  HtmlElement msg -> Bool
