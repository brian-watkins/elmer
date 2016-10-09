module Elmer.TestHelpers exposing (..)

import Elmer.Types exposing (..)

emptyNode : String -> HtmlNode
emptyNode tagName =
  { tag = tagName
  , id = Nothing
  , classes = Nothing
  , children = (HtmlElementList [])
  , events = Nothing
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
    { node | children = (HtmlElementList [(textNode text)]) }

nodeWithMultipleChildren : String -> HtmlNode
nodeWithMultipleChildren text =
  let
    node = emptyNode "div"
  in
    { node | children = (HtmlElementList [(textNode "fun stuff"), Node (emptyNode "div"), (textNode text)]) }
