module Elmer.Html.Matchers exposing
  ( hasText
  , hasClass
  , hasProperty
  , hasId
  )

{-| Make expectations about an Html element.

@docs hasText, hasId, hasClass, hasProperty

-}

import Elmer exposing (Matcher)
import Elmer.Internal exposing (..)
import Elmer.Html.Types exposing (..)
import Elmer.Html.Element as Element
import Elmer.Printer exposing (..)
import Expect
import String
import Json.Decode as Json

{-| Expect that an element has some text. This matcher will pass only if the element
or any of its descendents contains some `Html.text` with the specified text.
-}
hasText : String -> Matcher (HtmlElement msg)
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

{-| Expect that an element has the specified class. No need to prepend the class name with a dot.
-}
hasClass : String -> Matcher (HtmlElement msg)
hasClass className node =
    let
        classList =
            Element.classList node
    in
        if List.length classList > 0 then
            if List.member className classList then
                Expect.pass
            else
                Expect.fail (format [message "Expected node to have class" className, message "but it has" (printList classList) ])
        else
            Expect.fail (format [message "Expected node to have class" className, description "but it has no classes" ])

{-| Expect that an element has the specified property with the specified value.

    hasProperty ( "innerHtml", "Fun <i>stuff</i>" ) node

-}
hasProperty : (String, String) -> Matcher (HtmlElement msg)
hasProperty (name, value) node =
  case Element.property name node of
    Just propertyValue ->
      if value == propertyValue then
        Expect.pass
      else
        Expect.fail (format [message "Expected node to have property" (name ++ " = " ++ value),
          message "but it has" (name ++ " = " ++ propertyValue) ])
    Nothing ->
      Expect.fail (format [message "Expected node to have property" (name ++ " = " ++ value),
          description "but it has no property with that name" ])


{-| Expect that an element has the specified id. No need to prepend the id with a pound sign.
-}
hasId : String -> Matcher (HtmlElement msg)
hasId expectedId node =
  case Element.id node of
    Just nodeId ->
      if nodeId == expectedId then
        Expect.pass
      else
        Expect.fail (format [message "Expected node to have id" expectedId, message "but it has id" nodeId ])
    Nothing ->
      Expect.fail (format [message "Expected node to have id" expectedId, description "but it has no id" ])

-- Private functions

flattenTexts : List (HtmlNode msg) -> List String
flattenTexts children =
    List.concat <|
        List.map
            (\child ->
                case child of
                    Element n ->
                        flattenTexts n.children

                    Text t ->
                        [ t ]
            )
            children


printList : List String -> String
printList list =
    String.join ", " list
