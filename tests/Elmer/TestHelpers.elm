module Elmer.TestHelpers exposing (..)

import Elmer.Html.Types exposing (..)
import Dict

emptyNode : String -> HtmlElement msg
emptyNode tagName =
  { tag = tagName
  , facts = "{}"
  , children = []
  , inheritedEventHandlers = []
  , eventHandlers = []
  }

nodeWithClass : String -> HtmlElement msg
nodeWithClass className =
  let
    node = emptyNode "div"
  in
    { node | facts = "{\"className\":\"" ++ className ++ " funClass\"}"}

nodeWithId : String -> HtmlElement msg
nodeWithId id =
  let
    node = emptyNode "div"
  in
    { node | facts = "{\"id\":\"" ++ id ++ "\"}" }

nodeWithClassAndId : String -> String -> HtmlElement msg
nodeWithClassAndId className id =
  let
    node = emptyNode "div"
  in
    { node | facts = "{\"className\":\"" ++ className ++ " funClass\", \"id\":\"" ++ id ++ "\"}"}


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
  , facts = "{}"
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
    { node | facts = "{\"" ++ name ++ "\":\"" ++ value ++ "\"}" }

nodeWithBooleanProperty : (String, Bool) -> HtmlElement msg
nodeWithBooleanProperty (name, value) =
  let
    node = emptyNode "div"
  in
    { node | facts = "{\"" ++ name ++ "\":" ++ (String.toLower (toString value)) ++ "}" }
