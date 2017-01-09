module Elmer.Matchers exposing
  ( hasText
  , hasClass
  , hasProperty
  , hasId
  )

import Elmer.Types exposing (..)
import Elmer.Node as Node
import Elmer.Printer exposing (..)
import Expect
import String
import Json.Decode as Json

hasText : String -> Matcher (HtmlNode msg)
hasText text node =
    let
        texts =
            flattenTexts node.children
    in
        if List.length texts == 0 then
            Expect.fail (format [ message "Expected node to have text" text, description "but it has no text" ])
        else if List.member text texts then
            Expect.pass
        else
            Expect.fail (format [ message "Expected node to have text" text, message "but it has" (printList texts) ])


hasClass : String -> Matcher (HtmlNode msg)
hasClass className node =
    let
        classList =
            Node.classList node
    in
        if List.length classList > 0 then
            if List.member className classList then
                Expect.pass
            else
                Expect.fail (format [message "Expected node to have class" className, message "but it has" (printList classList) ])
        else
            Expect.fail (format [message "Expected node to have class" className, description "but it has no classes" ])


hasProperty : (String, String) -> Matcher (HtmlNode msg)
hasProperty (name, value) node =
  case Node.property name node of
    Just propertyValue ->
      if value == propertyValue then
        Expect.pass
      else
        Expect.fail (format [message "Expected node to have property" (name ++ " = " ++ value),
          message "but it has" (name ++ " = " ++ propertyValue) ])
    Nothing ->
      Expect.fail (format [message "Expected node to have property" (name ++ " = " ++ value),
          description "but it has no property with that name" ])

hasId : String -> Matcher (HtmlNode msg)
hasId expectedId node =
  case Node.id node of
    Just nodeId ->
      if nodeId == expectedId then
        Expect.pass
      else
        Expect.fail (format [message "Expected node to have id" expectedId, message "but it has id" nodeId ])
    Nothing ->
      Expect.fail (format [message "Expected node to have id" expectedId, description "but it has no id" ])

-- Private functions

flattenTexts : List (HtmlElement msg) -> List String
flattenTexts children =
    List.concat <|
        List.map
            (\child ->
                case child of
                    Node n ->
                        flattenTexts n.children

                    Text t ->
                        [ t ]
            )
            children


printList : List String -> String
printList list =
    String.join ", " list
