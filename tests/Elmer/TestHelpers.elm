module Elmer.TestHelpers exposing (..)

import Elmer.Types exposing (..)
import Dict

emptyNode : String -> HtmlNode msg
emptyNode tagName =
  { tag = tagName
  , facts = "{}"
  , children = []
  , events = []
  }

nodeWithClass : String -> HtmlNode msg
nodeWithClass className =
  let
    node = emptyNode "div"
  in
    { node | facts = "{\"className\":\"" ++ className ++ " funClass\"}"}

nodeWithId : String -> HtmlNode msg
nodeWithId id =
  let
    node = emptyNode "div"
  in
    { node | facts = "{\"id\":\"" ++ id ++ "\"}" }

textNode : String -> HtmlElement msg
textNode text =
  Text text

nodeWithText : String -> HtmlNode msg
nodeWithText text =
  let
    node = emptyNode "div"
  in
    { node | children = [(textNode text)] }

nodeWithMultipleChildren : String -> HtmlNode msg
nodeWithMultipleChildren text =
  let
    node = emptyNode "div"
  in
    { node | children = [(textNode "fun stuff"), Node (emptyNode "div"), (textNode text)] }

nodeWithNestedChildren : String -> HtmlNode msg
nodeWithNestedChildren text =
  { tag = "div"
  , facts = "{}"
  , children =
    [ (textNode "fun stuff")
    , Node (emptyNode "div")
    , (textNode "another sibling")
    , Node (nodeWithText text)
    ]
  , events = []
  }

nodeWithProperty : (String, String) -> HtmlNode msg
nodeWithProperty (name, value) =
  let
    node = emptyNode "div"
  in
    { node | facts = "{\"" ++ name ++ "\":\"" ++ value ++ "\"}" }
