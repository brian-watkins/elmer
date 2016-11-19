module Elmer.Matchers exposing (hasText, hasClass)

import Elmer exposing (..)
import Expect
import String

hasText : String -> HtmlNode msg -> Expect.Expectation
hasText text node =
  let
    texts = flattenTexts node.children
  in
    if List.length texts == 0 then
      Expect.fail ("Expected node to have text '" ++ text ++ "' but it has no text")
    else
      if List.member text texts then
        Expect.pass
      else
        Expect.fail ("Expected node to have text '" ++ text ++ "' but it has: " ++ (printList texts))

hasClass : String -> HtmlNode msg -> Expect.Expectation
hasClass className node =
  let
    classList = Elmer.classList node
  in
    if List.length classList > 0 then
      if List.member className classList then
        Expect.pass
      else
        Expect.fail ("Expected node to have class '" ++ className ++ "' but it has: " ++ (printList classList))
    else
      Expect.fail ("Expected node to have class '" ++ className ++ "' but it has no classes")

-- Private functions

flattenTexts : List (HtmlElement msg) -> List String
flattenTexts children =
  List.concat <|
    List.map (
      \child ->
        case child of
          Node n ->
            flattenTexts n.children
          Text t ->
            [ t ]
    ) children

printList : List String -> String
printList list =
  String.join ", " list
