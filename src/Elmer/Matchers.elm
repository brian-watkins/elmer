module Elmer.Matchers exposing (hasText, hasClass)

import Elmer.Types exposing (..)
import Expect
import String

hasText : String -> HtmlNode -> Expect.Expectation
hasText text node =
  let
    texts = List.filterMap extractText (unwrapElementList node.children)
  in
    if List.length texts == 0 then
      Expect.fail ("Expected node to have text '" ++ text ++ "' but it has no text")
    else
      if List.member text texts then
        Expect.pass
      else
        Expect.fail ("Expected node to have text '" ++ text ++ "' but it has: " ++ (printList texts))

hasClass : String -> HtmlNode -> Expect.Expectation
hasClass className node =
  case node.classes of
    Just classList ->
      if List.member className classList then
        Expect.pass
      else
        Expect.fail ("Expected node to have class '" ++ className ++ "' but it has: " ++ (printList classList))
    Nothing ->
      Expect.fail ("Expected node to have class '" ++ className ++ "' but it has no classes")

extractText : HtmlElement -> Maybe String
extractText element =
  case element of
    Node _ ->
      Nothing
    Text text ->
      Just text

printList : List String -> String
printList list =
  String.join ", " list

unwrapElementList : HtmlElementList -> List HtmlElement
unwrapElementList (HtmlElementList nodeList) =
  nodeList
