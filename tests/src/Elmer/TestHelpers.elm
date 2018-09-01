module Elmer.TestHelpers exposing (..)

import Elmer.Html.Types exposing (..)
import Dict exposing (Dict)
import Json.Decode as Json
import Html exposing (Html)
import Elmer.Html.Node as Node
import Elmer.Html.Printer as HtmlPrinter

-- REVISIT: Whoa this is a lot of implementation detail here ... 


printHtml : Html msg -> String
printHtml html =
  case Node.from html of
    Element element ->
      HtmlPrinter.toString element
    Text text ->
      "<No Elements>"


emptyNode : String -> HtmlElement msg
emptyNode tagName =
  { tag = tagName
  , properties = Dict.empty
  , attributes = Dict.empty
  , styles = Dict.empty
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
    | properties =
        factsWithStringValues [ ("className", className ++ " funClass") ]
    }

nodeWithId : String -> HtmlElement msg
nodeWithId id =
  let
    node = emptyNode "div"
  in
    { node | properties =
        factsWithStringValues [ ("id", id) ]
    }

nodeWithClassAndId : String -> String -> HtmlElement msg
nodeWithClassAndId className id =
  let
    node = emptyNode "div"
  in
    { node | properties =
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
  , properties = Dict.empty
  , attributes = Dict.empty
  , styles = Dict.empty
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
    { node | properties =
      factsWithStringValues [ (name, value) ]
    }

nodeWithBooleanProperty : (String, Bool) -> HtmlElement msg
nodeWithBooleanProperty (name, value) =
  let
    node = emptyNode "div"
  in
    { node | properties =
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
  , decoder = Json.succeed { message = "fakeEvent", stopPropagation = False, preventDefault = False }
  }
