module Elmer.TestHelpers exposing (..)

import Elmer.Html.Types exposing (..)
import Dict exposing (Dict)
import Json.Decode as Json

emptyNode : String -> HtmlElement msg
emptyNode tagName =
  { tag = tagName
  , facts = Dict.empty
  , children = []
  , inheritedEventHandlers = []
  , eventHandlers = []
  }

nodeWithClass : String -> HtmlElement msg
nodeWithClass className =
  let
    node = emptyNode "div"
  in
    { node
    | facts =
        factsWithStringValues [ ("className", className ++ " funClass") ]
    }

nodeWithId : String -> HtmlElement msg
nodeWithId id =
  let
    node = emptyNode "div"
  in
    { node | facts =
        factsWithStringValues [ ("id", id) ]
    }

nodeWithClassAndId : String -> String -> HtmlElement msg
nodeWithClassAndId className id =
  let
    node = emptyNode "div"
  in
    { node | facts =
      factsWithStringValues
        [ ("className", className ++ " funClass")
        , ("id", id)
        ]
    }

factsWithStringValues : List (String, String) -> Dict String HtmlFact
factsWithStringValues values =
  List.map (\(key, value) -> (key, StringValue value)) values
    |> Dict.fromList

factsWithBoolValues : List (String, Bool) -> Dict String HtmlFact
factsWithBoolValues values =
  List.map (\(key, value) -> (key, BoolValue value)) values
    |> Dict.fromList


textNode : String -> HtmlNode msg
textNode text =
  Text text

nodeWithText : String -> HtmlElement msg
nodeWithText text =
  let
    node = emptyNode "div"
  in
    { node | children = [(textNode text)] }

nodeWithList : HtmlElement msg
nodeWithList =
  let
    ul = emptyNode "ul"
  in
    { ul | children =
      [ Element (emptyNode "li")
      , Element (emptyNode "li")
      , Element (emptyNode "li")
      ]
    }

nodeWithMultipleChildren : String -> HtmlElement msg
nodeWithMultipleChildren text =
  let
    node = emptyNode "div"
  in
    { node | children = [(textNode "fun stuff"), Element (emptyNode "div"), (textNode text)] }

nodeWithNestedChildren : String -> HtmlElement msg
nodeWithNestedChildren text =
  { tag = "div"
  , facts = Dict.empty
  , children =
    [ (textNode "fun stuff")
    , Element (emptyNode "div")
    , (textNode "another sibling")
    , Element (nodeWithText text)
    ]
  , inheritedEventHandlers = []
  , eventHandlers = []
  }

nodeWithProperty : (String, String) -> HtmlElement msg
nodeWithProperty (name, value) =
  let
    node = emptyNode "div"
  in
    { node | facts =
      factsWithStringValues [ (name, value) ]
    }

nodeWithBooleanProperty : (String, Bool) -> HtmlElement msg
nodeWithBooleanProperty (name, value) =
  let
    node = emptyNode "div"
  in
    { node | facts =
      factsWithBoolValues [ (name, value) ]
    }

nodeWithEvents : List String -> HtmlElement String
nodeWithEvents events =
  let
    node = emptyNode "div"
    eventHandlers = List.map eventHandler events
  in
    { node | eventHandlers = eventHandlers }

eventHandler : String -> HtmlEventHandler String
eventHandler event =
  { eventType = event
  , options =
    { stopPropagation = False
    , preventDefault = False
    }
  , decoder = Json.succeed "fakeEvent"
  }
