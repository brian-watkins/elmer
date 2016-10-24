module Elmer.TestHelpers exposing (..)

import Elmer exposing (..)
import Dict

emptyNode : String -> HtmlNode
emptyNode tagName =
  { tag = tagName
  , facts = "{}"
  , children = []
  , events = []
  }

nodeWithClass : String -> HtmlNode
nodeWithClass className =
  let
    node = emptyNode "div"
  in
    { node | facts = "{\"className\":\"" ++ className ++ " funClass\"}"}

textNode : String -> HtmlElement
textNode text =
  Text text

nodeWithText : String -> HtmlNode
nodeWithText text =
  let
    node = emptyNode "div"
  in
    { node | children = [(textNode text)] }

nodeWithMultipleChildren : String -> HtmlNode
nodeWithMultipleChildren text =
  let
    node = emptyNode "div"
  in
    { node | children = [(textNode "fun stuff"), Node (emptyNode "div"), (textNode text)] }

nodeWithNestedChildren : String -> HtmlNode
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
