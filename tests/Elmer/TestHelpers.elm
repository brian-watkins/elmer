module Elmer.TestHelpers exposing (..)

import Elmer exposing (..)

emptyNode : String -> HtmlNode
emptyNode tagName =
  { tag = tagName
  , id = Nothing
  , classes = Nothing
  , children = []
  , events = []
  }

nodeWithClass : String -> HtmlNode
nodeWithClass className =
  let
    node = emptyNode "div"
  in
    { node | classes = Just [className, "funClass"] }

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
